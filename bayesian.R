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
library(dplyr)
library(tidyr)
library(INLA)

#Import the cleaned data
data <- read.csv("Maharashtra_Notification_Cleaned_Imputed_Final_15age.csv")

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
MH_sp$district_id <- 1:36
pop$district_id <- 1:36

MH_shapefile <- dplyr::select(MH_sp,district_id,District,geometry)

population <- pop %>% 
  pivot_longer(
    cols = starts_with("population_"),         # Selects population columns
    names_to = "Year",                          # New column name for years
    names_prefix = "population_",               # Removes the prefix "population_" from the year column
    values_to = "Population"
  )
population$Year <- as.numeric(population$Year)

summary <- data %>%
  group_by(District, Year) %>%
  summarize(
    incidence = n(),
    Treated = sum(Treatment_Outcome == "CURED" | Treatment_Outcome == "TREATMENT_COMPLETE" , na.rm = TRUE),
    Nontreated = sum(Treatment_Outcome == "DIED" , na.rm = TRUE),
    Diabetes = sum(Status_of_Diabetes == "Diabetic", na.rm = TRUE),
    Non_Diabetes = sum(Status_of_Diabetes == "Non-diabetic", na.rm = TRUE),
    HIV_pos = sum(Status_of_HIV == "Reactive" | Status_of_HIV == "Positive", na.rm = TRUE),
    HIV_neg = sum(Status_of_HIV == "Non-Reactive", na.rm = TRUE)
  ) %>%
  arrange(District, Year)

tb_summary <- summary %>%
  inner_join(population, by = c("District", "Year")) %>%
  mutate(incidence_rate = (incidence / Population) * 100000) %>%
  mutate(mortality_rate = (Nontreated / Population) * 100000) %>%
  mutate(mortality_frac = (Nontreated / incidence)) %>%
  mutate(diabetic_rate = (Diabetes/Population)*100000) %>% 
  mutate(diabetic_frac = (Diabetes/incidence)) %>%
  mutate(non_diabetic_rate = (Non_Diabetes/Population)*100000) %>% 
  mutate(non_diabetic_frac = (Non_Diabetes/incidence)) %>% 
  mutate(hiv_pos_rate = (HIV_pos/Population)*100000) %>% 
  mutate(hiv_pos_frac = (HIV_pos/incidence)) %>%
  mutate(hiv_neg_rate = (HIV_neg/Population)*100000) %>% 
  mutate(hiv_neg_frac = (HIV_neg/incidence))

district_map <- MH_shapefile %>%
  left_join(tb_summary, by = "district_id")



# Create adjacency matrix
nb <- poly2nb(MH_shapefile)
adjacency_matrix <- nb2mat(nb, style = "B", zero.policy = TRUE)

# Convert to INLA-readable format
adjacency_graph <- inla.read.graph(adjacency_matrix)


formula <- HIV_pos ~ f(district_id, model = "bym", graph = adjacency_graph) + 
  f(Year, model = "rw1") + 
  offset(log(Population))

# Run Bayesian modeling
result <- inla(
  formula,
  family = "poisson",
  data = district_map,
  control.predictor = list(compute = TRUE),
  control.compute = list(dic = TRUE, waic = TRUE)
)

# Summary of results
summary(result)

# Extract random effects
spatial_effect <- result$summary.random$district_id
temporal_effect <- result$summary.random$year

# Plot spatial effects
library(tmap)
district_map$spatial_effect <- spatial_effect$mean
tm_shape(district_map) +
  tm_polygons("spatial_effect", palette = "viridis", title = "Spatial Effect")

ggplot(data = district_map) +
  geom_sf(aes(fill = spatial_effect)) +
  scale_fill_viridis_c() +
  theme_minimal() +
  labs(title = "Spatial Effects of TB-HIV Cases", fill = "Effect")

predicted <- result$summary.fitted.values
district_map$predicted <- predicted$mean
ggplot(district_map, aes(x = HIV_pos, y = predicted)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, col = "red") +
  theme_minimal() +
  labs(title = "Observed vs Predicted TB-HIV Cases", x = "Observed", y = "Predicted")

district_map$residuals <- district_map$HIV_pos - district_map$predicted
ggplot(district_map, aes(x = longitude, y = latitude, col = residuals)) +
  geom_point() +
  theme_minimal() +
  labs(title = "Hotspot Analysis of Residuals")

ggplot(district_map) +
  geom_sf(aes(fill = residuals), color = "black") +
  #geom_sf_text(aes(label = district_map$District), fontface = "bold", size = 3.0)+
  scale_fill_gradient(low = "white", high = "red", name = "Mortality rate", limits = c(0,7)) +
  labs(title = "TB Mortality Rate per lakh across District", subtitle = "Year: 2023") +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(), # Remove axis text
    plot.title = element_text(size = 16, face = "bold"),  # Title styling  
    axis.ticks = element_blank(),   # Remove axis ticks
    panel.grid = element_blank()
  )


hiv <- dplyr::select(district_map, district_id,HIV_pos,predicted)
