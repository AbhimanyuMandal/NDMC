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

Jan_24 <- read_excel("NotificationRegister_2024_January_notification_date.xlsx")
colnames(Jan_24) <- Jan_24[5, ] ##Change the column name
Jan_24 <- Jan_24[-(1:5),] ##Remove the first 5 rows
Jan_24$Year <- 2024
Jan_24$Month <- "January"

Feb_24 <- read_excel("NotificationRegister_2024_February_notification_date.xlsx")
colnames(Feb_24) <- Feb_24[5, ] ##Change the column name
Feb_24 <- Feb_24[-(1:5),] ##Remove the first 5 rows
Feb_24$Year <- 2024
Feb_24$Month <- "February"

Mar_24 <- read_excel("NotificationRegister_2024_March_notification_date.xlsx")
colnames(Mar_24) <- Mar_24[5, ] ##Change the column name
Mar_24 <- Mar_24[-(1:5),] ##Remove the first 5 rows
Mar_24$Year <- 2024
Mar_24$Month <- "March"

Apr_24 <- read_excel("NotificationRegister_2024_April_notification_date.xlsx")
colnames(Apr_24) <- Apr_24[5, ] ##Change the column name
Apr_24 <- Apr_24[-(1:5),] ##Remove the first 5 rows
Apr_24$Year <- 2024
Apr_24$Month <- "April"

May_24 <- read_excel("NotificationRegister_2024_May_notification_date.xlsx")
colnames(May_24) <- May_24[5, ] ##Change the column name
May_24 <- May_24[-(1:5),] ##Remove the first 5 rows
May_24$Year <- 2024
May_24$Month <- "May"

Jun_24 <- read_excel("NotificationRegister_2024_June_notification_date.xlsx")
colnames(Jun_24) <- Jun_24[5, ] ##Change the column name
Jun_24 <- Jun_24[-(1:5),] ##Remove the first 5 rows
Jun_24$Year <- 2024
Jun_24$Month <- "June"

Jul_24 <- read_excel("NotificationRegister_2024_July_notification_date.xlsx")
colnames(Jul_24) <- Jul_24[5, ] ##Change the column name
Jul_24 <- Jul_24[-(1:5),] ##Remove the first 5 rows
Jul_24$Year <- 2024
Jul_24$Month <- "July"

Aug_24 <- read_excel("NotificationRegister_2024_August_notification_date.xlsx")
colnames(Aug_24) <- Aug_24[5, ] ##Change the column name
Aug_24 <- Aug_24[-(1:5),] ##Remove the first 5 rows
Aug_24$Year <- 2024
Aug_24$Month <- "August"

Sep_24 <- read_excel("NotificationRegister_2024_September_notification_date.xlsx")
colnames(Sep_24) <- Sep_24[5, ] ##Change the column name
Sep_24 <- Sep_24[-(1:5),] ##Remove the first 5 rows
Sep_24$Year <- 2024
Sep_24$Month <- "September"

Oct_24 <- read_excel("NotificationRegister_2024_October_notification_date.xlsx")
colnames(Oct_24) <- Oct_24[5, ] ##Change the column name
Oct_24 <- Oct_24[-(1:5),] ##Remove the first 5 rows
Oct_24$Year <- 2024
Oct_24$Month <- "October"

Nov_24 <- read_excel("NotificationRegister_2024_November_notification_date.xlsx")
colnames(Nov_24) <- Nov_24[5, ] ##Change the column name
Nov_24 <- Nov_24[-(1:5),] ##Remove the first 5 rows
Nov_24$Year <- 2024
Nov_24$Month <- "November"

Dec_24 <- read_excel("NotificationRegister_2024_December_notification_date.xlsx")
colnames(Dec_24) <- Dec_24[5, ] ##Change the column name
Dec_24 <- Dec_24[-(1:5),] ##Remove the first 5 rows
Dec_24$Year <- 2024
Dec_24$Month <- "December"

data <- rbind(Jan_24,Feb_24,Mar_24,Apr_24,May_24,Jun_24,Jul_24,Aug_24,Sep_24,Oct_24,Nov_24,Dec_24)

####Loading the spatial file with coordinates####
setwd("/Volumes/T7/")
INDIA <- readRDS('gadm36_IND_2_sf.rds') #shapefile 
MH_sp  <- subset(INDIA, NAME_1=="Maharashtra") ##Filter out the data from Maharashtra

####Loading the notification data of patients (2024-2024) and prepare them for further analysis####
#setwd("/Users//abhimanyu/Documents/Tuberculosis/Notification/Diagnosed/")
#data <- read_excel("NotificationRegister_2024_August_notification_date.xlsx") 
#colnames(data) <- data[5, ] ##Change the column name
#data <- data[-(1:5),] ##Remove the first 5 rows
#died  <- subset(data, Spectrum_Current_State == "Maharashtra") ##Filter out the data from Maharashtra

####Loading the population data of all the 36 districts in Maharashtra (2017-2024)####
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

summary_data <- summary_data %>% mutate(cases_per_capita = total_cases/pop$population_2023)

merged_data <- merge(MH_sp,summary_data, by = "District")

merged_data <- merged_data %>% filter(District!="Mumbai Suburban")
merged_data <- merged_data %>% filter(District!="Mumbai City")

centroids <- st_centroid(merged_data[1]$geometry)
centroids_coords <- as.data.frame(st_coordinates(centroids))

#TB-Diabetes

#Define a function to normalize values to a range [min, max]
normalize <- function(x, min_val, max_val) {
  (x - min(x)) / (max(x) - min(x)) * (max_val - min_val) + min_val
}

# Normalize the values in both dataframes to a scale [1, 100]
merged_data$Diabetic <- normalize(merged_data$Diabetes, 1, 100)
merged_data$Non_diabetic <- normalize(merged_data$Non_diabetes, 1, 100)

map1 <- ggplot(data = merged_data) +
  geom_sf(aes(fill = cases_per_capita), color = "black") +  # Choropleth
  scale_fill_gradient(low = "#FFFF66", high = "#CC3333", name = "TB Infection")+
  geom_text(aes(x = centroids_coords[,1], y = centroids_coords[,2], label = merged_data$District), size = 2.5, nudge_y = 0.1)+
  geom_point(data = merged_data,aes(x = centroids_coords[,1], y = centroids_coords[,2], size = Diabetic), color = "#0066CC", alpha = 0.6) +
  scale_size(range = c(1, 10)) +  # Adjust range to suit visual preference
  labs(title = "Bubble Map for TB + Diabetic patients (2024)",
       x = "Longitude",
       y = "Latitude") +
  theme_minimal()

map2 <- ggplot(data = merged_data) +
  geom_sf(aes(fill = cases_per_capita), color = "black") +  # Choropleth
  scale_fill_gradient(low = "#FFFF66", high = "#CC3333", name = "TB Infection")+
  geom_text(aes(x = centroids_coords[,1], y = centroids_coords[,2], label = merged_data$District), size = 2.5, nudge_y = 0.1)+
  geom_point(data = merged_data,aes(x = centroids_coords[,1], y = centroids_coords[,2], size = Non_diabetic), color = "#333333", alpha = 0.6) +
  scale_size(range = c(1, 10)) +  # Adjust range to suit visual preference
  labs(title = "Bubble Map for TB + Non-Diabetic patients (2024)",
       x = "Longitude",
       y = "Latitude") +
  theme_minimal()

library(gridExtra)
grid.arrange(map1, map2, ncol = 2)


#TB-HIV

#Define a function to normalize values to a range [min, max]
normalize <- function(x, min_val, max_val) {
  (x - min(x)) / (max(x) - min(x)) * (max_val - min_val) + min_val
}

# Normalize the values in both dataframes to a scale [1, 100]
merged_data$Reactivee <- normalize(merged_data$Reactive, 1, 100)
merged_data$Non_reactivee <- normalize(merged_data$Non_reactive, 1, 100)
merged_data$Positivee <- normalize(merged_data$Positive, 1, 100)


map3 <- ggplot(data = merged_data) +
  geom_sf(aes(fill = cases_per_capita), color = "black") +  # Choropleth
  scale_fill_gradient(low = "#FFFF66", high = "#CC3333", name = "TB Infection")+
  geom_text(aes(x = centroids_coords[,1], y = centroids_coords[,2], label = merged_data$District), size = 2.5, nudge_y = 0.1)+  
  geom_point(data = merged_data,aes(x = centroids_coords[,1], y = centroids_coords[,2], size = Reactivee), color = "#833d60",
             alpha = 0.6) +# Bubble plot
  scale_size(range = c(1, 4)) +  # Adjust range to suit visual preference
  labs(title = "Bubble Map for TB + Reactive HIV patients (2024)",
       x = "Longitude",
       y = "Latitude") +
  theme_minimal()

map4 <- ggplot(data = merged_data) +
  geom_sf(aes(fill = cases_per_capita), color = "black") +  # Choropleth
  scale_fill_gradient(low = "#FFFF66", high = "#CC3333", name = "TB Infection")+
  geom_text(aes(x = centroids_coords[,1], y = centroids_coords[,2], label = merged_data$District), size = 2.5, nudge_y = 0.1)+  
  geom_point(data = merged_data,aes(x = centroids_coords[,1], y = centroids_coords[,2], size = Non_reactivee), color = "#3d8360",
             alpha = 0.6) +# Bubble plot
  scale_size(range = c(1, 4)) +  # Adjust range to suit visual preference
  labs(title = "Bubble Map for TB + Non-Reactive HIV patients (2024)",
       x = "Longitude",
       y = "Latitude") +
  theme_minimal()

map5 <- ggplot(data = merged_data) +
  geom_sf(aes(fill = cases_per_capita), color = "black") +  # Choropleth
  scale_fill_gradient(low = "#FFFF66", high = "#CC3333", name = "TB Infection")+
  geom_text(aes(x = centroids_coords[,1], y = centroids_coords[,2], label = merged_data$District), size = 2.5, nudge_y = 0.1)+  
  geom_point(data = merged_data,aes(x = centroids_coords[,1], y = centroids_coords[,2], size = Positivee), color = "#3d3d83",
             alpha = 0.6) +# Bubble plot
  scale_size(range = c(1, 4)) +  # Adjust range to suit visual preference
  labs(title = "Bubble Map for TB + Positive HIV patients (2024)",
       x = "Longitude",
       y = "Latitude") +
  theme_minimal()


# Display the maps side-by-side
library(gridExtra)
grid.arrange(map3, map4, map5, ncol = 2)
  
