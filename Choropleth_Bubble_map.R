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
library(spdep)
library(raster)
library(sp)
library(sf)
library(readxl)
library(dplyr) 

##Change directory containing files
setwd("/Volumes/T7/Notification/Diagnosed/")

Jan_22 <- read_excel("NotificationRegister_2022_January_notification_date.xlsx")
colnames(Jan_22) <- Jan_22[5, ] ##Change the column name
Jan_22 <- Jan_22[-(1:5),] ##Remove the first 5 rows
Jan_22$Year <- 2022
Jan_22$Month <- "January"

Feb_22 <- read_excel("NotificationRegister_2022_February_notification_date.xlsx")
colnames(Feb_22) <- Feb_22[5, ] ##Change the column name
Feb_22 <- Feb_22[-(1:5),] ##Remove the first 5 rows
Feb_22$Year <- 2022
Feb_22$Month <- "February"

Mar_22 <- read_excel("NotificationRegister_2022_March_notification_date.xlsx")
colnames(Mar_22) <- Mar_22[5, ] ##Change the column name
Mar_22 <- Mar_22[-(1:5),] ##Remove the first 5 rows
Mar_22$Year <- 2022
Mar_22$Month <- "March"

Apr_22 <- read_excel("NotificationRegister_2022_April_notification_date.xlsx")
colnames(Apr_22) <- Apr_22[5, ] ##Change the column name
Apr_22 <- Apr_22[-(1:5),] ##Remove the first 5 rows
Apr_22$Year <- 2022
Apr_22$Month <- "April"

May_22 <- read_excel("NotificationRegister_2022_May_notification_date.xlsx")
colnames(May_22) <- May_22[5, ] ##Change the column name
May_22 <- May_22[-(1:5),] ##Remove the first 5 rows
May_22$Year <- 2022
May_22$Month <- "May"

Jun_22 <- read_excel("NotificationRegister_2022_June_notification_date.xlsx")
colnames(Jun_22) <- Jun_22[5, ] ##Change the column name
Jun_22 <- Jun_22[-(1:5),] ##Remove the first 5 rows
Jun_22$Year <- 2022
Jun_22$Month <- "June"

Jul_22 <- read_excel("NotificationRegister_2022_July_notification_date.xlsx")
colnames(Jul_22) <- Jul_22[5, ] ##Change the column name
Jul_22 <- Jul_22[-(1:5),] ##Remove the first 5 rows
Jul_22$Year <- 2022
Jul_22$Month <- "July"

Aug_22 <- read_excel("NotificationRegister_2022_August_notification_date.xlsx")
colnames(Aug_22) <- Aug_22[5, ] ##Change the column name
Aug_22 <- Aug_22[-(1:5),] ##Remove the first 5 rows
Aug_22$Year <- 2022
Aug_22$Month <- "August"

Sep_22 <- read_excel("NotificationRegister_2022_September_notification_date.xlsx")
colnames(Sep_22) <- Sep_22[5, ] ##Change the column name
Sep_22 <- Sep_22[-(1:5),] ##Remove the first 5 rows
Sep_22$Year <- 2022
Sep_22$Month <- "September"

Oct_22 <- read_excel("NotificationRegister_2022_October_notification_date.xlsx")
colnames(Oct_22) <- Oct_22[5, ] ##Change the column name
Oct_22 <- Oct_22[-(1:5),] ##Remove the first 5 rows
Oct_22$Year <- 2022
Oct_22$Month <- "October"

Nov_22 <- read_excel("NotificationRegister_2022_November_notification_date.xlsx")
colnames(Nov_22) <- Nov_22[5, ] ##Change the column name
Nov_22 <- Nov_22[-(1:5),] ##Remove the first 5 rows
Nov_22$Year <- 2022
Nov_22$Month <- "November"

Dec_22 <- read_excel("NotificationRegister_2022_December_notification_date.xlsx")
colnames(Dec_22) <- Dec_22[5, ] ##Change the column name
Dec_22 <- Dec_22[-(1:5),] ##Remove the first 5 rows
Dec_22$Year <- 2022
Dec_22$Month <- "December"

data <- rbind(Jan_22,Feb_22,Mar_22,Apr_22,May_22,Jun_22,Jul_22,Aug_22,Sep_22,Oct_22,Nov_22,Dec_22)
####Loading the spatial file with coordinates####
setwd("/Volumes/T7/")
INDIA <- readRDS('gadm36_IND_2_sf.rds') #shapefile 
MH_sp  <- subset(INDIA, NAME_1=="Maharashtra") ##Filter out the data from Maharashtra

####Loading the notification data of patients (2022-2022) and prepare them for further analysis####
#setwd("/Users//abhimanyu/Documents/Tuberculosis/Notification/Diagnosed/")
#data <- read_excel("NotificationRegister_2022_August_notification_date.xlsx") 
#colnames(data) <- data[5, ] ##Change the column name
#data <- data[-(1:5),] ##Remove the first 5 rows
#died  <- subset(data, Spectrum_Current_State == "Maharashtra") ##Filter out the data from Maharashtra

####Loading the population data of all the 36 districts in Maharashtra (2017-2022)####
pop <- read_excel("population.xlsx")
data  <- subset(data, Spectrum_Current_State == "Maharashtra") ##Filter out the data from Maharashtra

####Data Preprocessing####
N1=length(unique(data$Spectrum_Current_District)) ##number of districts before cleaning
data$Spectrum_Current_District <- gsub("^Mumbai.*", "Mumbai Suburban", data$Spectrum_Current_District)
data$Spectrum_Current_District <- gsub(".*MC$", "Mumbai City", data$Spectrum_Current_District)
data$Spectrum_Current_District <- gsub("Pune Rural", "Pune", data$Spectrum_Current_District)
data$Spectrum_Current_District <- gsub("PUNE RURAL", "Pune", data$Spectrum_Current_District)
data$Spectrum_Current_District <- gsub("LATUR", "Latur", data$Spectrum_Current_District)
data$Spectrum_Current_District <- gsub("CHANDRAPUR", "Chandrapur", data$Spectrum_Current_District)
data$Spectrum_Current_District <- gsub("PARBHANI", "Parbhani", data$Spectrum_Current_District)
data$Spectrum_Current_District <- gsub("SINDHUDURG", "Sindhudurg", data$Spectrum_Current_District)
data$Spectrum_Current_District <- gsub("NAGPUR", "Nagpur", data$Spectrum_Current_District)
data$Spectrum_Current_District <- gsub("JALNA", "Jalna", data$Spectrum_Current_District)
data$Spectrum_Current_District <- gsub("NANDED", "Nanded", data$Spectrum_Current_District)
MH_sp$NAME_2 <- gsub("Bid", "Beed", MH_sp$NAME_2)
MH_sp$NAME_2 <- gsub("Garhchiroli", "Gadchiroli", MH_sp$NAME_2)
MH_sp$NAME_2 <- gsub("Raigarh", "Raigad", MH_sp$NAME_2)
MH_sp$NAME_2 <- gsub("Aurangabad","Chhatrapati Sambhajinagar", MH_sp$NAME_2)
MH_sp$NAME_2 <- gsub("Osmanabad","Dharashiv", MH_sp$NAME_2)
pop$District <- gsub("Osmanabad","Dharashiv", pop$District)

N1=length(unique(data$Spectrum_Current_District)) ##number of districts after cleaning (~36)

####Ensure same column name for districts in both the files####
names(MH_sp)[names(MH_sp) == "NAME_2"] <- "District"
names(data)[names(data) == "Spectrum_Current_District"] <- "District"

summary_data <- data %>%
  group_by(District) %>%
  summarise(
    total_cases = n(),
    Treated = sum(Treatment_Outcome == "CURED" | Treatment_Outcome == "TREATMENT_COMPLETE" , na.rm = TRUE),
    Diabetes = sum(Status_of_Diabetes == "Diabetic", na.rm = TRUE),
    Non_diabetes = sum(Status_of_Diabetes == "Non-diabetic", na.rm = TRUE),
    Reactive = sum(Status_of_HIV == "Reactive", na.rm = TRUE),
    Non_reactive = sum(Status_of_HIV == "Non-Reactive", na.rm = TRUE),
    Positive = sum(Status_of_HIV == "Positive", na.rm = TRUE)
  )

summary_data <- summary_data %>% mutate(cases_per_capita = total_cases/pop$population_2022)

merged_data <- merge(MH_sp,summary_data, by = "District")

centroids <- st_centroid(merged_data[1]$geometry)
centroids_coords <- st_coordinates(centroids)


#summary_data2 <- data %>%
# group_by(District, Status_of_Diabetes) %>%
#summarise(count = n()) %>%
#ungroup()

#merged_data2 <- merge(MH_sp,summary_data2, by = "District")
  
  
  max_count <- max(c(merged_data$Diabetes, merged_data$Non_diabetes))

map1 <- ggplot(data = merged_data) +
  geom_sf(aes(fill = cases_per_capita), color = "white") +  # Choropleth
  scale_fill_gradient(low = "lightblue", high = "red", name = "TB Infection")+  
  geom_point(data = merged_data,
             aes(x = centroids_coords[,1], y = centroids_coords[,2], size = Diabetes), color = "orange",
             alpha = 0.6) +# Bubble plot
  scale_size_continuous(range = c(3, 12), limits = c(0, max_count)) +
  #scale_color_manual(values = c("green"), name = "Diabetes Status") +
  theme_minimal() +
  theme(legend.position = "right")

map2 <- ggplot(data = merged_data) +
  geom_sf(aes(fill = cases_per_capita), color = "white") +  # Choropleth
  scale_fill_gradient(low = "lightblue", high = "red", name = "TB Infection")+  
  geom_point(data = merged_data,
             aes(x = centroids_coords[,1], y = centroids_coords[,2], size = Non_diabetes), color = "purple",
             alpha = 0.6) +# Bubble plot
  scale_size_continuous(range = c(3, 12), limits = c(0, max_count)) +
  #scale_color_manual(values = c("green"), name = "Diabetes Status") +
  theme_minimal() +
  theme(legend.position = "right")


# Display the maps side-by-side
library(gridExtra)
grid.arrange(map1, map2, ncol = 2)


max_count <- max(c(merged_data$Reactive, merged_data$Non_reactive,merged_data$Positive))

map3 <- ggplot(data = merged_data) +
  geom_sf(aes(fill = cases_per_capita), color = "white") +  # Choropleth
  scale_fill_gradient(low = "lightblue", high = "red", name = "TB Infection")+  
  geom_point(data = merged_data,
             aes(x = centroids_coords[,1], y = centroids_coords[,2], size = Reactive), color = "pink",
             alpha = 0.6) +# Bubble plot
  scale_size_continuous(range = c(3, 12), limits = c(0, max_count)) +
  #scale_color_manual(values = c("green"), name = "Diabetes Status") +
  theme_minimal() +
  theme(legend.position = "right")

map4 <- ggplot(data = merged_data) +
  geom_sf(aes(fill = cases_per_capita), color = "white") +  # Choropleth
  scale_fill_gradient(low = "lightblue", high = "red", name = "TB Infection")+  
  geom_point(data = merged_data,
             aes(x = centroids_coords[,1], y = centroids_coords[,2], size = Non_reactive), color = "orange",
             alpha = 0.6) +# Bubble plot
  scale_size_continuous(range = c(3, 12), limits = c(0, max_count)) +
  #scale_color_manual(values = c("green"), name = "Diabetes Status") +
  theme_minimal() +
  theme(legend.position = "right")

map5 <- ggplot(data = merged_data) +
  geom_sf(aes(fill = cases_per_capita), color = "white") +  # Choropleth
  scale_fill_gradient(low = "lightblue", high = "red", name = "TB Infection")+  
  geom_point(data = merged_data,
             aes(x = centroids_coords[,1], y = centroids_coords[,2], size = Positive), color = "green",
             alpha = 0.6) +# Bubble plot
  scale_size_continuous(range = c(3, 12), limits = c(0, max_count)) +
  #scale_color_manual(values = c("green"), name = "Diabetes Status") +
  theme_minimal() +
  theme(legend.position = "right")


# Display the maps side-by-side
library(gridExtra)
grid.arrange(map3, map4, map5, ncol = 3)
  
