##Libraries 
library(forecast)
library(xts)
library(zoo)
library(aTSA)
library(tscount)
library(spData)
library(sf)
library(spdep)###
library(ggplot2)
library(scales)
#library(statmod)
library(spdep)
library(raster)####
library(sp)
library(sf) ###
#install.packages("readxl")
library(readxl)
library(dplyr)
#install.packages("openxlsx")
library(openxlsx)

setwd("/Users/hp/Documents/SME_lab/")
INDIA <- readRDS('gadm36_IND_2_sf.rds') #shapefile 
MH_sp  <- subset(INDIA, NAME_1=="Maharashtra") 


##spatial analysis
setwd("/Users/hp/Documents/SME_lab/NDMC_TB/Diagnosed/")

data_22 <- read_excel("NotificationRegister_2024_August_notification_date.xlsx") 
colnames(data_22) <- data_22[5, ]
data_22 <- data_22[-(1:5),]
mh_data_22  <- subset(data_22, Spectrum_Current_State == "Maharashtra") 

#Comparison between current and diagnosed districts
data_221 <- data_22[,c("Spectrum_Current_District", "Spectrum_Diagnosing_District")]
data_221 <- data_221 %>% mutate(comparison = Spectrum_Current_District == Spectrum_Diagnosing_District)

#data cleaning
N1=length(unique(mh_data_22$Spectrum_Current_District)) #number of districts
mh_data_22$Spectrum_Current_District <- gsub("^Mumbai.*", "Mumbai Suburban", mh_data_22$Spectrum_Current_District)
mh_data_22$Spectrum_Current_District <- gsub(".*MC$", "Mumbai City", mh_data_22$Spectrum_Current_District)
mh_data_22$Spectrum_Current_District <- gsub("Pune Rural", "Pune", mh_data_22$Spectrum_Current_District)
mh_data_22$Spectrum_Current_District <- gsub("PUNE RURAL", "Pune", mh_data_22$Spectrum_Current_District)
mh_data_22$Spectrum_Current_District <- gsub("LATUR", "Latur", mh_data_22$Spectrum_Current_District)
mh_data_22$Spectrum_Current_District <- gsub("CHANDRAPUR", "Chandrapur", mh_data_22$Spectrum_Current_District)
mh_data_22$Spectrum_Current_District <- gsub("PARBHANI", "Parbhani", mh_data_22$Spectrum_Current_District)
mh_data_22$Spectrum_Current_District <- gsub("SINDHUDURG", "Sindhudurg", mh_data_22$Spectrum_Current_District)
mh_data_22$Spectrum_Current_District <- gsub("NAGPUR", "Nagpur", mh_data_22$Spectrum_Current_District)
mh_data_22$Spectrum_Current_District <- gsub("JALNA", "Jalna", mh_data_22$Spectrum_Current_District)
mh_data_22$Spectrum_Current_District <- gsub("NANDED", "Nanded", mh_data_22$Spectrum_Current_District)
MH_sp$NAME_2 <- gsub("Bid", "Beed", MH_sp$NAME_2)
MH_sp$NAME_2 <- gsub("Garhchiroli", "Gadchiroli", MH_sp$NAME_2)
MH_sp$NAME_2 <- gsub("Raigarh", "Raigad", MH_sp$NAME_2)
MH_sp$NAME_2 <- gsub("Aurangabad","Chhatrapati Sambhajinagar", MH_sp$NAME_2)
MH_sp$NAME_2 <- gsub("Osmanabad","Dharashiv", MH_sp$NAME_2)

N1=length(unique(mh_data_22$Spectrum_Current_District)) #number of districts
mh_data_22_copy <- mh_data_22

#Getting the shapefile for the data
#shape_mh <- MH_sp[,c("NAME_2","geometry")]
names(MH_sp)[names(MH_sp) == "NAME_2"] <- "District"
names(mh_data_22_copy)[names(mh_data_22_copy) == "Spectrum_Current_District"] <- "District"


merged_data <- merge(MH_sp,mh_data_22_copy, by="District")

# Create neighbor list based on spatial contiguity (using queen criterion)
neighbors <- poly2nb(merged_data)

# Create spatial weights matrix from neighbors
weights <- nb2listw(neighbors, style = "W", zero.policy = TRUE)

# Run Moran's I test for global spatial autocorrelation
global_moran <- moran.test(merged_data$infection_cases, weights, zero.policy = TRUE)

# Run Local Moran's I (LISA)
local_moran <- localmoran(merged_data$infection_cases, weights, zero.policy = TRUE)

# Append LISA results to the merged data
merged_data$local_moran_I <- local_moran[, 1]  # Local Moran's I values
merged_data$p_value <- local_moran[, 5]  # P-values of the local test

# Quadrants for Local Moran's I
merged_data$quadrant <- NA
mean_cases <- mean(merged_data$infection_cases, na.rm = TRUE)

for (i in 1:nrow(merged_data)) {
  if (merged_data$infection_cases[i] > mean_cases & local_moran[i, 1] > 0) {
    merged_data$quadrant[i] <- "High-High"
  } else if (merged_data$infection_cases[i] > mean_cases & local_moran[i, 1] < 0) {
    merged_data$quadrant[i] <- "High-Low"
  } else if (merged_data$infection_cases[i] < mean_cases & local_moran[i, 1] > 0) {
    merged_data$quadrant[i] <- "Low-Low"
  } else if (merged_data$infection_cases[i] < mean_cases & local_moran[i, 1] < 0) {
    merged_data$quadrant[i] <- "Low-High"
  }
}

# Visualize using ggplot2
ggplot(data = merged_data) +
  geom_sf(aes(fill = quadrant)) +
  scale_fill_manual(values = c("High-High" = "red", "High-Low" = "orange", "Low-Low" = "blue", "Low-High" = "green")) +
  labs(title = "LISA Cluster Map",
       fill = "Cluster Type") +
  theme_minimal()
