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
library(CARBayes)
install.packages("INLA", repos = "https://inla.r-inla-download.org/R/stable", dep = TRUE)
library(INLA)       # For Bayesian inference using INLA

#Import the cleaned data
data <- read.csv("C:/Users/MDMC (Workshop)/Documents/Maharashtra_Notification_Cleaned_Imputed_Final.csv")

####Loading the spatial file with coordinates####
INDIA <- readRDS('gadm36_IND_2_sf.rds') #shapefile 
MH_sp  <- subset(INDIA, NAME_1=="Maharashtra") ##Filter out the data from Maharashtra


####Loading the population data of all the 36 districts in Maharashtra (2017-2022)####
pop <- read_excel("C:/Users/MDMC (Workshop)/Documents/population_maharashtra.xlsx")
data  <- subset(data, Spectrum_Diagnosing_State == "Maharashtra") ##Filter out the data from Maharashtra

####Data Preprocessing####
N1=length(unique(data$Spectrum_Diagnosing_District)) ##number of districts before cleaning
data$Spectrum_Diagnosing_District <- gsub("^Mumbai.*", "Mumbai Suburban", data$Spectrum_Diagnosing_District)
data$Spectrum_Diagnosing_District <- gsub(".*MC$", "Mumbai City", data$Spectrum_Diagnosing_District)
data$Spectrum_Diagnosing_District <- gsub("Pune Rural", "Pune", data$Spectrum_Diagnosing_District)
data$Spectrum_Diagnosing_District <- gsub("PUNE RURAL", "Pune", data$Spectrum_Diagnosing_District)
data$Spectrum_Diagnosing_District <- gsub("LATUR", "Latur", data$Spectrum_Diagnosing_District)
data$Spectrum_Diagnosing_District <- gsub("CHANDRAPUR", "Chandrapur", data$Spectrum_Diagnosing_District)
data$Spectrum_Diagnosing_District <- gsub("PARBHANI", "Parbhani", data$Spectrum_Diagnosing_District)
data$Spectrum_Diagnosing_District <- gsub("SINDHUDURG", "Sindhudurg", data$Spectrum_Diagnosing_District)
data$Spectrum_Diagnosing_District <- gsub("NAGPUR", "Nagpur", data$Spectrum_Diagnosing_District)
data$Spectrum_Diagnosing_District <- gsub("JALNA", "Jalna", data$Spectrum_Diagnosing_District)
data$Spectrum_Diagnosing_District <- gsub("NANDED", "Nanded", data$Spectrum_Diagnosing_District)
MH_sp$NAME_2 <- gsub("Bid", "Beed", MH_sp$NAME_2)
MH_sp$NAME_2 <- gsub("Garhchiroli", "Gadchiroli", MH_sp$NAME_2)
MH_sp$NAME_2 <- gsub("Raigarh", "Raigad", MH_sp$NAME_2)
MH_sp$NAME_2 <- gsub("Aurangabad","Chhatrapati Sambhajinagar", MH_sp$NAME_2)
MH_sp$NAME_2 <- gsub("Osmanabad","Dharashiv", MH_sp$NAME_2)
pop$District <- gsub("Osmanabad","Dharashiv", pop$District)

N1=length(unique(data$Spectrum_Diagnosing_District)) ##number of districts after cleaning (~36)

####Ensure same column name for districts in both the files####
names(MH_sp)[names(MH_sp) == "NAME_2"] <- "District"
names(data)[names(data) == "Spectrum_Diagnosing_District"] <- "District"


summary_data <- data %>%
  group_by(District) %>%
  summarise(
    total_cases = n(),
    Treated = sum(Treatment_Outcome == "CURED" | Treatment_Outcome == "TREATMENT_COMPLETE" , na.rm = TRUE),
    Untreated = sum(Treatment_Outcome == "DIED"),
    Diabetic_status = sum(Status_of_Diabetes == "Diabetic", na.rm = TRUE),
    Non_diabetic_status = sum(Status_of_Diabetes == "Non-diabetic", na.rm = TRUE),
    Reactive = sum(Status_of_HIV == "Reactive", na.rm = TRUE),
    Non_reactive = sum(Status_of_HIV == "Non-Reactive", na.rm = TRUE),
    Positive = sum(Status_of_HIV == "Positive", na.rm = TRUE)
  )

merged_data <- merge(MH_sp,pop,by="District")
merged_data <- merge(merged_data,summary_data,by="District")

merged_data_sp <- as(merged_data, "Spatial")

# Create an adjacency matrix using spatial contiguity
neighbors <- poly2nb(merged_data_sp)
W <- nb2mat(neighbors, style = "B", zero.policy = TRUE)


car_model <- S.CARbym(formula = Treated ~ offset(log(total_cases)),
                      family = "poisson",
                      data = merged_data_sp@data,
                      W = W,
                      burnin = 50000, n.sample = 1000000, thin = 100)

summary(car_model)
#View(car_model)

merged_data$posterior_risk <- car_model$summary.results[, "Mean"]
plot(merged_data["posterior_risk"])









# Define a spatial model with INLA
formula <- HIV_status ~ f(District, model = "bym", graph = W) + offset(log(population_2022))

# Fit the model using INLA
inla_model <- inla(formula,
                   family = "poisson",
                   data = merged_data_sp@data,
                   E = merged_data_sp@data$population_2022,
                   control.predictor = list(compute = TRUE),verbose=TRUE )
