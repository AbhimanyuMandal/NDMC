####Loading libraries####
library(forecast)
library(xts)
library(zoo)
library(aTSA)
library(tscount)
library(spData)
library(sf)
library(spdep)
library(ggplot2)
library(scales)
#library(statmod)
library(spdep)
library(raster)
library(sp)
library(sf)
library(readxl)
library(dplyr)

####Loading the spatial file with coordinates####
setwd("/Users/hp/Documents/SME_lab/")
INDIA <- readRDS('gadm36_IND_2_sf.rds') #shapefile 
MH_sp  <- subset(INDIA, NAME_1=="Maharashtra") ##Filter out the data from Maharashtra

####Loading the notification data of patients (2021-2024) and prepare them for further analysis####
setwd("/Users/hp/Documents/SME_lab/NDMC_TB/Diagnosed/")
data <- read_excel("NotificationRegister_2024_August_notification_date.xlsx") 
colnames(data) <- data[5, ] ##Change the column name
data <- data[-(1:5),] ##Remove the first 5 rows
mh_data  <- subset(data, Spectrum_Current_State == "Maharashtra") ##Filter out the data from Maharashtra

####Loading the population data of all the 36 districts in Maharashtra (2017-2023)####
pop <- read_excel("/Users/hp/Documents/SME_lab/NDMC_TB/Diagnosed/Hotspot/population.xlsx")

####Data Preprocessing####
N1=length(unique(mh_data$Spectrum_Current_District)) ##number of districts before cleaning
mh_data$Spectrum_Current_District <- gsub("^Mumbai.*", "Mumbai Suburban", mh_data$Spectrum_Current_District)
mh_data$Spectrum_Current_District <- gsub(".*MC$", "Mumbai City", mh_data$Spectrum_Current_District)
mh_data$Spectrum_Current_District <- gsub("Pune Rural", "Pune", mh_data$Spectrum_Current_District)
mh_data$Spectrum_Current_District <- gsub("PUNE RURAL", "Pune", mh_data$Spectrum_Current_District)
mh_data$Spectrum_Current_District <- gsub("LATUR", "Latur", mh_data$Spectrum_Current_District)
mh_data$Spectrum_Current_District <- gsub("CHANDRAPUR", "Chandrapur", mh_data$Spectrum_Current_District)
mh_data$Spectrum_Current_District <- gsub("PARBHANI", "Parbhani", mh_data$Spectrum_Current_District)
mh_data$Spectrum_Current_District <- gsub("SINDHUDURG", "Sindhudurg", mh_data$Spectrum_Current_District)
mh_data$Spectrum_Current_District <- gsub("NAGPUR", "Nagpur", mh_data$Spectrum_Current_District)
mh_data$Spectrum_Current_District <- gsub("JALNA", "Jalna", mh_data$Spectrum_Current_District)
mh_data$Spectrum_Current_District <- gsub("NANDED", "Nanded", mh_data$Spectrum_Current_District)
MH_sp$NAME_2 <- gsub("Bid", "Beed", MH_sp$NAME_2)
MH_sp$NAME_2 <- gsub("Garhchiroli", "Gadchiroli", MH_sp$NAME_2)
MH_sp$NAME_2 <- gsub("Raigarh", "Raigad", MH_sp$NAME_2)
MH_sp$NAME_2 <- gsub("Aurangabad","Chhatrapati Sambhajinagar", MH_sp$NAME_2)
MH_sp$NAME_2 <- gsub("Osmanabad","Dharashiv", MH_sp$NAME_2)
N1=length(unique(mh_data$Spectrum_Current_District)) ##number of districts after cleaning (~36)

####Ensure same column name for districts in both the files####
names(MH_sp)[names(MH_sp) == "NAME_2"] <- "District"
names(mh_data)[names(mh_data) == "Spectrum_Current_District"] <- "District"


####Aggregate infection data by district####
district_infection_count <- mh_data %>% group_by(District) %>% summarise(num_cases = n())  # Count the number of cases per district

####Merge the infection count and total population data####
data <- merge(district_infection_count,pop)

####Normalizing the infection count in each district with population count####
data <- data %>% mutate(cases_per_capita = num_cases/data$population_2023)

####Final merging of normalized data with the spatial file of Maharashtra####
map_data <- MH_sp %>% left_join(data, by = c("District" = "District"))

####Extract the coordinates for each district in Maharashtra####
centroids <- st_centroid(MH_sp[1]$geometry)
centroids_coords <- st_coordinates(centroids)

####Calculate quantiles for classification of infection data into three categories - Low, Medium, High####
quantiles <- quantile(map_data$cases_per_capita, probs = c(0.25, 0.75), na.rm = TRUE)

##Categorize based on quantiles
map_data <- map_data %>%
  mutate(infection_level = case_when(
    cases_per_capita >= quantiles[2] ~ "High",
    cases_per_capita <= quantiles[1] ~ "Low",
    TRUE ~ "Medium"
  ))


####Create the choropleth plot####
ggplot(map_data) +
  geom_sf(aes(fill = infection_level), color = "white") +
  geom_text(aes(x = centroids_coords[,1], y = centroids_coords[,2], label = map_data$District), size = 2.5, nudge_y = 0.1)+
  #scale_fill_gradient(low = "lightblue", high = "red", name = "Cases per Capita") +
  scale_fill_manual(values = c("Low" = "lightblue", "Medium" = "lightyellow", "High" = "lightpink", name = "Infection Level"))+
  labs(title = "Infection Cases per Capita by State in August 2024",
       subtitle = "Normalized by Population",
       caption = "Data source: your data") +
  theme_minimal()
