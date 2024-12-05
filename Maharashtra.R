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
library(tidyr)
library(ggplot2)
        
        
data <- read.csv("Maharashtra_Notification_Cleaned_Imputed_Final.csv")
data  <- subset(data, Spectrum_Diagnosing_State == "Maharashtra") ##Filter out the data from Maharashtra
INDIA <- readRDS('gadm36_IND_2_sf.rds') #shapefile 
MH_sp  <- subset(INDIA, NAME_1=="Maharashtra") ##Filter out the data from Maharashtra
pop <- read_excel("population_maharashtra.xlsx")


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
    Diabetic_status = sum(Status_of_Diabetes == "Diabetic", na.rm = TRUE),
    Non_diabetic_status = sum(Status_of_Diabetes == "Non-diabetic", na.rm = TRUE),
    Reactive = sum(Status_of_HIV == "Reactive", na.rm = TRUE),
    Non_reactive = sum(Status_of_HIV == "Non-Reactive", na.rm = TRUE),
    Positive = sum(Status_of_HIV == "Positive", na.rm = TRUE)
    ) %>%
  arrange(District, Year)


tb_summary <- summary %>%
  inner_join(population, by = c("District", "Year")) %>%
  mutate(incidence_rate = (incidence / Population) * 100000) %>%
  mutate(mortality_rate = (Nontreated / Population) * 100000) %>%
  mutate(mortality_rate_frac = (Nontreated / incidence)*100) %>%
  mutate(diabetic_rate = (Diabetic_status/Population)*100000) %>% 
  mutate(diabetic_rate_frac = (Diabetic_status/incidence)) %>%
  mutate(non_diabetic_rate = (Non_diabetic_status/Population)*100000) %>% 
  mutate(non_diabetic_rate_frac = (Non_diabetic_status/incidence)) 

district_map <- MH_sp %>%
  left_join(tb_summary, by = "District")

district_map <- district_map %>%
  mutate(mortality_rate_frac = round(mortality_rate_frac, 2))

# Create the label column for district names with cases
district_map <- district_map %>%
  mutate(CFR = paste0(District, " (", mortality_rate_frac, ")"))

####Extract the coordinates for each district in Maharashtra####
centroids <- st_centroid(MH_sp[1]$geometry)
centroids_coords <- st_coordinates(centroids)















library(ggplot2)

#incidence choropleth plot
plot_map <- district_map %>% filter(Year == 2021)
plot_map <- ggplot(plot_map) +
  geom_sf(aes(fill = incidence_rate), color = "black") +
  geom_sf_text(aes(label = MH_sp$District), fontface = "bold", size = 3.0)+
  scale_fill_gradient(low = "yellow", high = "red", name = "Incidence rate", limits = c(0, 250)) +
  labs(title = "TB Incidence Rate per lakh across District", subtitle = "Year: 2021") +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(), # Remove axis text
    plot.title = element_text(size = 16, face = "bold"),  # Title styling  
    axis.ticks = element_blank(),   # Remove axis ticks
    panel.grid = element_blank()
  )
ggsave(
  filename = "C:/Users/MDMC (Workshop)/Documents/final_maharashtra/incidence_21.png",
  plot = plot_map,  # Replace with your ggplot object
  width = 10, height = 8,   # Width and height in inches
  dpi = 1000                 # Resolution in dots per inch
)


#mortality choropleth plot
plot_map <- district_map %>% filter(Year == 2023)
plot_map <- ggplot(plot_map) +
  geom_sf(aes(fill = mortality_rate), color = "black") +
  geom_sf_text(aes(label = MH_sp$District), fontface = "bold", size = 3.0)+
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
ggsave(
  filename = "C:/Users/MDMC (Workshop)/Documents/final_maharashtra/mortality_23.png",
  plot = plot_map,  # Replace with your ggplot object
  width = 10, height = 8,   # Width and height in inches
  dpi = 600                 # Resolution in dots per inch
)



incidence_21 <- district_map %>% filter(Year == 2021)
incidence_22 <- district_map %>% filter(Year == 2022)
incidence_23 <- district_map %>% filter(Year == 2023)


incidence_23$cfr1 <-  ((incidence_23$Nontreated/incidence_22$incidence)*100)
incidence_23 <- incidence_23 %>%
  mutate(cfr1 = round(cfr1, 2))

# Create the label column for district names with cases
incidence_23 <- incidence_23 %>%
  mutate(cfr11 = paste0(District, " (", cfr1, ")"))




#CFR choropleth plot
#plot_map <- district_map %>% filter(Year == 2023)
plot_map11 <- ggplot(incidence_23) +
  geom_sf(aes(fill = cfr1), color = "black") +
  geom_sf_text(aes(label = incidence_23$cfr11), fontface = "bold", size = 3.0)+
  scale_fill_gradient(low = "white", high = "red", name = "CFR (%)", limits = c(1.90,11.00)) +
  labs(title = "TB-CFR across District", subtitle = "Year: 2023") +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(), # Remove axis text
    plot.title = element_text(size = 16, face = "bold"),  # Title styling  
    axis.ticks = element_blank(),   # Remove axis ticks
    panel.grid = element_blank()
  )
ggsave(
  filename = "C:/Users/MDMC (Workshop)/Documents/final_maharashtra/CFR_23__1.png",
  plot = plot_map11,  # Replace with your ggplot object
  width = 10, height = 8,   # Width and height in inches
  dpi = 1000                 # Resolution in dots per inch
)




incidence_22$cfr1 <-  ((incidence_22$Nontreated/incidence_21$incidence)*100)
incidence_22 <- incidence_22 %>%
  mutate(cfr1 = round(cfr1, 2))

# Create the label column for district names with cases
incidence_22 <- incidence_22 %>%
  mutate(cfr11 = paste0(District, " (", cfr1, ")"))




#CFR choropleth plot
#plot_map <- district_map %>% filter(Year == 2023)
plot_map11 <- ggplot(incidence_22) +
  geom_sf(aes(fill = cfr1), color = "black") +
  geom_sf_text(aes(label = incidence_22$cfr11), fontface = "bold", size = 3.0)+
  scale_fill_gradient(low = "white", high = "red", name = "CFR (%)", limits = c(1.90,11.00)) +
  labs(title = "TB-CFR across District", subtitle = "Year: 2022") +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(), # Remove axis text
    plot.title = element_text(size = 16, face = "bold"),  # Title styling  
    axis.ticks = element_blank(),   # Remove axis ticks
    panel.grid = element_blank()
  )
ggsave(
  filename = "C:/Users/MDMC (Workshop)/Documents/final_maharashtra/CFR_22__1.png",
  plot = plot_map11,  # Replace with your ggplot object
  width = 10, height = 8,   # Width and height in inches
  dpi = 1000                 # Resolution in dots per inch
)















merged_data1 <- district_map %>% filter(District!="Mumbai City")
merged_data11 <- merged_data1 %>% filter(District!="Mumbai Suburban")

merged_data <- merged_data11 %>% filter(Year == 2021)

#merged_data <- merge(MH_sp,data, by="District")

# Create neighbor list based on spatial contiguity (using queen criterion)
neighbors <- poly2nb(merged_data)

# Create spatial weights matrix from neighbors
weights <- nb2listw(neighbors, style = "W", zero.policy = TRUE)

# Run Moran's I test for global spatial autocorrelation
global_moran <- moran.test(merged_data$incidence_rate, weights, zero.policy = TRUE)

# Run Local Moran's I (LISA)
local_moran <- localmoran(merged_data$incidence_rate, weights, zero.policy = TRUE)

# Append LISA results to the merged data
merged_data$local_moran_I <- local_moran[, 1]  # Local Moran's I values
merged_data$p_value <- local_moran[, 5]  # P-values of the local test

# Quadrants for Local Moran's I
merged_data$quadrant <- NA
mean_cases <- mean(merged_data$incidence_rate, na.rm = TRUE)

for (i in 1:nrow(merged_data)) {
  if (merged_data$incidence_rate[i] > mean_cases & local_moran[i, 1] > 0) {
    merged_data$quadrant[i] <- "High-High"
  } else if (merged_data$incidence_rate[i] > mean_cases & local_moran[i, 1] < 0) {
    merged_data$quadrant[i] <- "High-Low"
  } else if (merged_data$incidence_rate[i] < mean_cases & local_moran[i, 1] > 0) {
    merged_data$quadrant[i] <- "Low-Low"
  } else if (merged_data$incidence_rate[i] < mean_cases & local_moran[i, 1] < 0) {
    merged_data$quadrant[i] <- "Low-High"
  }
}
# Filter for significant p-values
merged_data$significant_cluster <- ifelse(merged_data$p_value < 0.05, merged_data$quadrant, NA)

#Visualize using ggplot2
plot_map1 <- ggplot(data = merged_data) +
geom_sf(aes(fill = significant_cluster)) +
scale_fill_manual(values = c("High-High" = "red", "High-Low" = "orange", "Low-Low" = "blue", "Low-High" = "green")) +
labs(title = "TB incidence LISA Cluster Map", subtitle = "Based on Diagnosing District", fill = "Cluster Type") +
theme_minimal()+
  theme(
  axis.title = element_blank(),
  axis.text = element_blank(), # Remove axis text
  plot.title = element_text(size = 16, face = "bold"),  # Title styling  
  axis.ticks = element_blank(),   # Remove axis ticks
  panel.grid = element_blank()
)
ggsave(
  filename = "C:/Users/MDMC (Workshop)/Documents/final_maharashtra/LISA(D)_21.png",
  plot = plot_map1,  # Replace with your ggplot object
  width = 10, height = 8,   # Width and height in inches
  dpi = 1000                 # Resolution in dots per inch
)




merged_data <- merged_data11 %>% filter(Year == 2022)

#merged_data <- merge(MH_sp,data, by="District")

# Create neighbor list based on spatial contiguity (using queen criterion)
neighbors <- poly2nb(merged_data)

# Create spatial weights matrix from neighbors
weights <- nb2listw(neighbors, style = "W", zero.policy = TRUE)

# Run Moran's I test for global spatial autocorrelation
global_moran <- moran.test(merged_data$incidence_rate, weights, zero.policy = TRUE)

# Run Local Moran's I (LISA)
local_moran <- localmoran(merged_data$incidence_rate, weights, zero.policy = TRUE)

# Append LISA results to the merged data
merged_data$local_moran_I <- local_moran[, 1]  # Local Moran's I values
merged_data$p_value <- local_moran[, 5]  # P-values of the local test

# Quadrants for Local Moran's I
merged_data$quadrant <- NA
mean_cases <- mean(merged_data$incidence_rate, na.rm = TRUE)

for (i in 1:nrow(merged_data)) {
  if (merged_data$incidence_rate[i] > mean_cases & local_moran[i, 1] > 0) {
    merged_data$quadrant[i] <- "High-High"
  } else if (merged_data$incidence_rate[i] > mean_cases & local_moran[i, 1] < 0) {
    merged_data$quadrant[i] <- "High-Low"
  } else if (merged_data$incidence_rate[i] < mean_cases & local_moran[i, 1] > 0) {
    merged_data$quadrant[i] <- "Low-Low"
  } else if (merged_data$incidence_rate[i] < mean_cases & local_moran[i, 1] < 0) {
    merged_data$quadrant[i] <- "Low-High"
  }
}
# Filter for significant p-values
merged_data$significant_cluster <- ifelse(merged_data$p_value < 0.05, merged_data$quadrant, NA)

#Visualize using ggplot2
plot_map1 <- ggplot(data = merged_data) +
  geom_sf(aes(fill = significant_cluster)) +
  scale_fill_manual(values = c("High-High" = "red", "High-Low" = "orange", "Low-Low" = "blue", "Low-High" = "green")) +
  labs(title = "TB incidence LISA Cluster Map", subtitle = "Based on Diagnosing District", fill = "Cluster Type") +
  theme_minimal()+
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(), # Remove axis text
    plot.title = element_text(size = 16, face = "bold"),  # Title styling  
    axis.ticks = element_blank(),   # Remove axis ticks
    panel.grid = element_blank()
  )
ggsave(
  filename = "C:/Users/MDMC (Workshop)/Documents/final_maharashtra/LISA(D)_22.png",
  plot = plot_map1,  # Replace with your ggplot object
  width = 10, height = 8,   # Width and height in inches
  dpi = 1000                 # Resolution in dots per inch
)


merged_data <- merged_data11 %>% filter(Year == 2023)

#merged_data <- merge(MH_sp,data, by="District")

# Create neighbor list based on spatial contiguity (using queen criterion)
neighbors <- poly2nb(merged_data)

# Create spatial weights matrix from neighbors
weights <- nb2listw(neighbors, style = "W", zero.policy = TRUE)

# Run Moran's I test for global spatial autocorrelation
global_moran <- moran.test(merged_data$incidence_rate, weights, zero.policy = TRUE)

# Run Local Moran's I (LISA)
local_moran <- localmoran(merged_data$incidence_rate, weights, zero.policy = TRUE)

# Append LISA results to the merged data
merged_data$local_moran_I <- local_moran[, 1]  # Local Moran's I values
merged_data$p_value <- local_moran[, 5]  # P-values of the local test

# Quadrants for Local Moran's I
merged_data$quadrant <- NA
mean_cases <- mean(merged_data$incidence_rate, na.rm = TRUE)

for (i in 1:nrow(merged_data)) {
  if (merged_data$incidence_rate[i] > mean_cases & local_moran[i, 1] > 0) {
    merged_data$quadrant[i] <- "High-High"
  } else if (merged_data$incidence_rate[i] > mean_cases & local_moran[i, 1] < 0) {
    merged_data$quadrant[i] <- "High-Low"
  } else if (merged_data$incidence_rate[i] < mean_cases & local_moran[i, 1] > 0) {
    merged_data$quadrant[i] <- "Low-Low"
  } else if (merged_data$incidence_rate[i] < mean_cases & local_moran[i, 1] < 0) {
    merged_data$quadrant[i] <- "Low-High"
  }
}
# Filter for significant p-values
merged_data$significant_cluster <- ifelse(merged_data$p_value < 0.05, merged_data$quadrant, NA)

#Visualize using ggplot2
plot_map1 <- ggplot(data = merged_data) +
  geom_sf(aes(fill = significant_cluster)) +
  scale_fill_manual(values = c("High-High" = "red", "High-Low" = "orange", "Low-Low" = "blue", "Low-High" = "green")) +
  labs(title = "TB incidence LISA Cluster Map", subtitle = "Based on Diagnosing District", fill = "Cluster Type") +
  theme_minimal()+
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(), # Remove axis text
    plot.title = element_text(size = 16, face = "bold"),  # Title styling  
    axis.ticks = element_blank(),   # Remove axis ticks
    panel.grid = element_blank()
  )
ggsave(
  filename = "C:/Users/MDMC (Workshop)/Documents/final_maharashtra/LISA(D)_23.png",
  plot = plot_map1,  # Replace with your ggplot object
  width = 10, height = 8,   # Width and height in inches
  dpi = 1000                 # Resolution in dots per inch
)


merged_data <- merged_data11 %>% filter(Year == 2021)

#merged_data <- merge(MH_sp,data, by="District")

# Create neighbor list based on spatial contiguity (using queen criterion)
neighbors <- poly2nb(merged_data)

# Create spatial weights matrix from neighbors
weights <- nb2listw(neighbors, style = "W", zero.policy = TRUE)

# Run Moran's I test for global spatial autocorrelation
global_moran <- moran.test(merged_data$mortality_rate, weights, zero.policy = TRUE)

# Run Local Moran's I (LISA)
local_moran <- localmoran(merged_data$mortality_rate, weights, zero.policy = TRUE)

# Append LISA results to the merged data
merged_data$local_moran_I <- local_moran[, 1]  # Local Moran's I values
merged_data$p_value <- local_moran[, 5]  # P-values of the local test

# Quadrants for Local Moran's I
merged_data$quadrant <- NA
mean_cases <- mean(merged_data$mortality_rate, na.rm = TRUE)

for (i in 1:nrow(merged_data)) {
  if (merged_data$mortality_rate[i] > mean_cases & local_moran[i, 1] > 0) {
    merged_data$quadrant[i] <- "High-High"
  } else if (merged_data$mortality_rate[i] > mean_cases & local_moran[i, 1] < 0) {
    merged_data$quadrant[i] <- "High-Low"
  } else if (merged_data$mortality_rate[i] < mean_cases & local_moran[i, 1] > 0) {
    merged_data$quadrant[i] <- "Low-Low"
  } else if (merged_data$mortality_rate[i] < mean_cases & local_moran[i, 1] < 0) {
    merged_data$quadrant[i] <- "Low-High"
  }
}
# Filter for significant p-values
merged_data$significant_cluster <- ifelse(merged_data$p_value < 0.05, merged_data$quadrant, NA)

#Visualize using ggplot2
plot_map1 <- ggplot(data = merged_data) +
  geom_sf(aes(fill = significant_cluster)) +
  scale_fill_manual(values = c("High-High" = "red", "High-Low" = "orange", "Low-Low" = "blue", "Low-High" = "green")) +
  labs(title = "TB mortality LISA Cluster Map", subtitle = "Based on Diagnosing District", fill = "Cluster Type") +
  theme_minimal()+
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(), # Remove axis text
    plot.title = element_text(size = 16, face = "bold"),  # Title styling  
    axis.ticks = element_blank(),   # Remove axis ticks
    panel.grid = element_blank()
  )
ggsave(
  filename = "C:/Users/MDMC (Workshop)/Documents/final_maharashtra/LISA(D)M_21.png",
  plot = plot_map1,  # Replace with your ggplot object
  width = 10, height = 8,   # Width and height in inches
  dpi = 1000                 # Resolution in dots per inch
)




merged_data <- merged_data11 %>% filter(Year == 2022)

#merged_data <- merge(MH_sp,data, by="District")

# Create neighbor list based on spatial contiguity (using queen criterion)
neighbors <- poly2nb(merged_data)

# Create spatial weights matrix from neighbors
weights <- nb2listw(neighbors, style = "W", zero.policy = TRUE)

# Run Moran's I test for global spatial autocorrelation
global_moran <- moran.test(merged_data$mortality_rate, weights, zero.policy = TRUE)

# Run Local Moran's I (LISA)
local_moran <- localmoran(merged_data$mortality_rate, weights, zero.policy = TRUE)

# Append LISA results to the merged data
merged_data$local_moran_I <- local_moran[, 1]  # Local Moran's I values
merged_data$p_value <- local_moran[, 5]  # P-values of the local test

# Quadrants for Local Moran's I
merged_data$quadrant <- NA
mean_cases <- mean(merged_data$mortality_rate, na.rm = TRUE)

for (i in 1:nrow(merged_data)) {
  if (merged_data$mortality_rate[i] > mean_cases & local_moran[i, 1] > 0) {
    merged_data$quadrant[i] <- "High-High"
  } else if (merged_data$mortality_rate[i] > mean_cases & local_moran[i, 1] < 0) {
    merged_data$quadrant[i] <- "High-Low"
  } else if (merged_data$mortality_rate[i] < mean_cases & local_moran[i, 1] > 0) {
    merged_data$quadrant[i] <- "Low-Low"
  } else if (merged_data$mortality_rate[i] < mean_cases & local_moran[i, 1] < 0) {
    merged_data$quadrant[i] <- "Low-High"
  }
}
# Filter for significant p-values
merged_data$significant_cluster <- ifelse(merged_data$p_value < 0.05, merged_data$quadrant, NA)

#Visualize using ggplot2
plot_map1 <- ggplot(data = merged_data) +
  geom_sf(aes(fill = significant_cluster)) +
  scale_fill_manual(values = c("High-High" = "red", "High-Low" = "orange", "Low-Low" = "blue", "Low-High" = "green")) +
  labs(title = "TB mortality LISA Cluster Map", subtitle = "Based on Diagnosing District", fill = "Cluster Type") +
  theme_minimal()+
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(), # Remove axis text
    plot.title = element_text(size = 16, face = "bold"),  # Title styling  
    axis.ticks = element_blank(),   # Remove axis ticks
    panel.grid = element_blank()
  )
ggsave(
  filename = "C:/Users/MDMC (Workshop)/Documents/final_maharashtra/LISA(D)M_22.png",
  plot = plot_map1,  # Replace with your ggplot object
  width = 10, height = 8,   # Width and height in inches
  dpi = 1000                 # Resolution in dots per inch
)


merged_data <- merged_data11 %>% filter(Year == 2023)

#merged_data <- merge(MH_sp,data, by="District")

# Create neighbor list based on spatial contiguity (using queen criterion)
neighbors <- poly2nb(merged_data)

# Create spatial weights matrix from neighbors
weights <- nb2listw(neighbors, style = "W", zero.policy = TRUE)

# Run Moran's I test for global spatial autocorrelation
global_moran <- moran.test(merged_data$mortality_rate, weights, zero.policy = TRUE)

# Run Local Moran's I (LISA)
local_moran <- localmoran(merged_data$mortality_rate, weights, zero.policy = TRUE)

# Append LISA results to the merged data
merged_data$local_moran_I <- local_moran[, 1]  # Local Moran's I values
merged_data$p_value <- local_moran[, 5]  # P-values of the local test

# Quadrants for Local Moran's I
merged_data$quadrant <- NA
mean_cases <- mean(merged_data$mortality_rate, na.rm = TRUE)

for (i in 1:nrow(merged_data)) {
  if (merged_data$mortality_rate[i] > mean_cases & local_moran[i, 1] > 0) {
    merged_data$quadrant[i] <- "High-High"
  } else if (merged_data$mortality_rate[i] > mean_cases & local_moran[i, 1] < 0) {
    merged_data$quadrant[i] <- "High-Low"
  } else if (merged_data$mortality_rate[i] < mean_cases & local_moran[i, 1] > 0) {
    merged_data$quadrant[i] <- "Low-Low"
  } else if (merged_data$mortality_rate[i] < mean_cases & local_moran[i, 1] < 0) {
    merged_data$quadrant[i] <- "Low-High"
  }
}
# Filter for significant p-values
merged_data$significant_cluster <- ifelse(merged_data$p_value < 0.05, merged_data$quadrant, NA)

#Visualize using ggplot2
plot_map1 <- ggplot(data = merged_data) +
  geom_sf(aes(fill = significant_cluster)) +
  scale_fill_manual(values = c("High-High" = "red", "High-Low" = "orange", "Low-Low" = "blue", "Low-High" = "green")) +
  labs(title = "TB mortality LISA Cluster Map", subtitle = "Based on Diagnosing District", fill = "Cluster Type") +
  theme_minimal()+
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(), # Remove axis text
    plot.title = element_text(size = 16, face = "bold"),  # Title styling  
    axis.ticks = element_blank(),   # Remove axis ticks
    panel.grid = element_blank()
  )
ggsave(
  filename = "C:/Users/MDMC (Workshop)/Documents/final_maharashtra/LISA(D)M_23.png",
  plot = plot_map1,  # Replace with your ggplot object
  width = 10, height = 8,   # Width and height in inches
  dpi = 1000                 # Resolution in dots per inch
)



##################################################################################

data <- read.csv("Maharashtra_Notification_Cleaned_Imputed_Final.csv")
data  <- subset(data, Spectrum_Residence_State == "Maharashtra") ##Filter out the data from Maharashtra
INDIA <- readRDS('gadm36_IND_2_sf.rds') #shapefile 
MH_sp  <- subset(INDIA, NAME_1=="Maharashtra") ##Filter out the data from Maharashtra
pop <- read_excel("population_maharashtra.xlsx")


####Data Preprocessing####
N1=length(unique(data$Spectrum_Residence_District)) ##number of districts before cleaning
data$Spectrum_Residence_District <- gsub("^Mumbai.*", "Mumbai Suburban", data$Spectrum_Residence_District)
data$Spectrum_Residence_District <- gsub(".*MC$", "Mumbai City", data$Spectrum_Residence_District)
data$Spectrum_Residence_District <- gsub("Pune Rural", "Pune", data$Spectrum_Residence_District)
data$Spectrum_Residence_District <- gsub("PUNE RURAL", "Pune", data$Spectrum_Residence_District)
data$Spectrum_Residence_District <- gsub("LATUR", "Latur", data$Spectrum_Residence_District)
data$Spectrum_Residence_District <- gsub("CHANDRAPUR", "Chandrapur", data$Spectrum_Residence_District)
data$Spectrum_Residence_District <- gsub("PARBHANI", "Parbhani", data$Spectrum_Residence_District)
data$Spectrum_Residence_District <- gsub("SINDHUDURG", "Sindhudurg", data$Spectrum_Residence_District)
data$Spectrum_Residence_District <- gsub("NAGPUR", "Nagpur", data$Spectrum_Residence_District)
data$Spectrum_Residence_District <- gsub("JALNA", "Jalna", data$Spectrum_Residence_District)
data$Spectrum_Residence_District <- gsub("NANDED", "Nanded", data$Spectrum_Residence_District)
MH_sp$NAME_2 <- gsub("Bid", "Beed", MH_sp$NAME_2)
MH_sp$NAME_2 <- gsub("Garhchiroli", "Gadchiroli", MH_sp$NAME_2)
MH_sp$NAME_2 <- gsub("Raigarh", "Raigad", MH_sp$NAME_2)
MH_sp$NAME_2 <- gsub("Aurangabad","Chhatrapati Sambhajinagar", MH_sp$NAME_2)
MH_sp$NAME_2 <- gsub("Osmanabad","Dharashiv", MH_sp$NAME_2)
pop$District <- gsub("Osmanabad","Dharashiv", pop$District)

N1=length(unique(data$Spectrum_Residence_District)) ##number of districts after cleaning (~36)

####Ensure same column name for districts in both the files####
names(MH_sp)[names(MH_sp) == "NAME_2"] <- "District"
names(data)[names(data) == "Spectrum_Residence_District"] <- "District"

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
    Diabetic_status = sum(Status_of_Diabetes == "Diabetic", na.rm = TRUE),
    Non_diabetic_status = sum(Status_of_Diabetes == "Non-diabetic", na.rm = TRUE),
    Reactive = sum(Status_of_HIV == "Reactive", na.rm = TRUE),
    Non_reactive = sum(Status_of_HIV == "Non-Reactive", na.rm = TRUE),
    Positive = sum(Status_of_HIV == "Positive", na.rm = TRUE)
  ) %>%
  arrange(District, Year)


tb_summary <- summary %>%
  inner_join(population, by = c("District", "Year")) %>%
  mutate(incidence_rate = (incidence / Population) * 100000) %>%
  mutate(mortality_rate = (Nontreated / Population) * 100000) %>%
  mutate(mortality_rate_frac = (Nontreated / incidence)*100) %>%
  mutate(diabetic_rate = (Diabetic_status/Population)*100000) %>% 
  mutate(diabetic_rate_frac = (Diabetic_status/incidence)) %>%
  mutate(non_diabetic_rate = (Non_diabetic_status/Population)*100000) %>% 
  mutate(non_diabetic_rate_frac = (Non_diabetic_status/incidence)) 

district_map <- MH_sp %>%
  left_join(tb_summary, by = "District")

district_map <- district_map %>%
  mutate(mortality_rate_frac = round(mortality_rate_frac, 2))

# Create the label column for district names with cases
district_map <- district_map %>%
  mutate(CFR = paste0(District, " (", mortality_rate_frac, ")"))

####Extract the coordinates for each district in Maharashtra####
centroids <- st_centroid(MH_sp[1]$geometry)
centroids_coords <- st_coordinates(centroids)



plot_map <- district_map %>% filter(Year == 2024) 
#incidence choropleth plot
library(ggplot2)
ggplot(plot_map) +
  geom_sf(aes(fill = incidence_rate)) +
  geom_text(aes(x = centroids_coords[,1], y = centroids_coords[,2], label = MH_sp$District), size = 2.3)+
  scale_fill_gradient(low = "yellow", high = "red", name = "Incidence", limits = c(0, 250)) +
  labs(title = "TB Incidence Rate by District", subtitle = "Year: 2023") +
  theme_minimal()


plot_map <- district_map %>% filter(Year == 2024)

#mortality choropleth plot
library(ggplot2)
ggplot(plot_map) +
  geom_sf(aes(fill = mortality_rate)) +
  geom_text(aes(x = centroids_coords[,1], y = centroids_coords[,2], label = MH_sp$District), size = 2.3)+
  scale_fill_gradient(low = "white", high = "red", name = "Mortality rate", limits = c(20,233 )) +
  labs(title = "TB Mortality Rate by District", subtitle = "Year: 2024") +
  theme_minimal()


merged_data1 <- district_map %>% filter(District!="Mumbai City")
merged_data11 <- merged_data1 %>% filter(District!="Mumbai Suburban")

merged_data <- merged_data11 %>% filter(Year == 2021)

#merged_data <- merge(MH_sp,data, by="District")

# Create neighbor list based on spatial contiguity (using queen criterion)
neighbors <- poly2nb(merged_data)

# Create spatial weights matrix from neighbors
weights <- nb2listw(neighbors, style = "W", zero.policy = TRUE)

# Run Moran's I test for global spatial autocorrelation
global_moran <- moran.test(merged_data$mortality_rate, weights, zero.policy = TRUE)

# Run Local Moran's I (LISA)
local_moran <- localmoran(merged_data$mortality_rate, weights, zero.policy = TRUE)

# Append LISA results to the merged data
merged_data$local_moran_I <- local_moran[, 1]  # Local Moran's I values
merged_data$p_value <- local_moran[, 5]  # P-values of the local test

# Quadrants for Local Moran's I
merged_data$quadrant <- NA
mean_cases <- mean(merged_data$mortality_rate, na.rm = TRUE)

for (i in 1:nrow(merged_data)) {
  if (merged_data$mortality_rate[i] > mean_cases & local_moran[i, 1] > 0) {
    merged_data$quadrant[i] <- "High-High"
  } else if (merged_data$mortality_rate[i] > mean_cases & local_moran[i, 1] < 0) {
    merged_data$quadrant[i] <- "High-Low"
  } else if (merged_data$mortality_rate[i] < mean_cases & local_moran[i, 1] > 0) {
    merged_data$quadrant[i] <- "Low-Low"
  } else if (merged_data$mortality_rate[i] < mean_cases & local_moran[i, 1] < 0) {
    merged_data$quadrant[i] <- "Low-High"
  }
}
# Filter for significant p-values
merged_data$significant_cluster <- ifelse(merged_data$p_value < 0.05, merged_data$quadrant, NA)

#Visualize using ggplot2
plot_map1 <- ggplot(data = merged_data) +
  geom_sf(aes(fill = significant_cluster)) +
  scale_fill_manual(values = c("High-High" = "red", "High-Low" = "orange", "Low-Low" = "blue", "Low-High" = "green")) +
  labs(title = "TB mortality LISA Cluster Map", subtitle = "Based on Residing District", fill = "Cluster Type") +
  theme_minimal()+
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(), # Remove axis text
    plot.title = element_text(size = 16, face = "bold"),  # Title styling  
    axis.ticks = element_blank(),   # Remove axis ticks
    panel.grid = element_blank()
  )
ggsave(
  filename = "C:/Users/MDMC (Workshop)/Documents/final_maharashtra/LISA(R)M_21.png",
  plot = plot_map1,  # Replace with your ggplot object
  width = 10, height = 8,   # Width and height in inches
  dpi = 1000                 # Resolution in dots per inch
)






merged_data <- merged_data11 %>% filter(Year == 2022)

#merged_data <- merge(MH_sp,data, by="District")

# Create neighbor list based on spatial contiguity (using queen criterion)
neighbors <- poly2nb(merged_data)

# Create spatial weights matrix from neighbors
weights <- nb2listw(neighbors, style = "W", zero.policy = TRUE)

# Run Moran's I test for global spatial autocorrelation
global_moran <- moran.test(merged_data$mortality_rate, weights, zero.policy = TRUE)

# Run Local Moran's I (LISA)
local_moran <- localmoran(merged_data$mortality_rate, weights, zero.policy = TRUE)

# Append LISA results to the merged data
merged_data$local_moran_I <- local_moran[, 1]  # Local Moran's I values
merged_data$p_value <- local_moran[, 5]  # P-values of the local test

# Quadrants for Local Moran's I
merged_data$quadrant <- NA
mean_cases <- mean(merged_data$mortality_rate, na.rm = TRUE)

for (i in 1:nrow(merged_data)) {
  if (merged_data$mortality_rate[i] > mean_cases & local_moran[i, 1] > 0) {
    merged_data$quadrant[i] <- "High-High"
  } else if (merged_data$mortality_rate[i] > mean_cases & local_moran[i, 1] < 0) {
    merged_data$quadrant[i] <- "High-Low"
  } else if (merged_data$mortality_rate[i] < mean_cases & local_moran[i, 1] > 0) {
    merged_data$quadrant[i] <- "Low-Low"
  } else if (merged_data$mortality_rate[i] < mean_cases & local_moran[i, 1] < 0) {
    merged_data$quadrant[i] <- "Low-High"
  }
}
# Filter for significant p-values
merged_data$significant_cluster <- ifelse(merged_data$p_value < 0.05, merged_data$quadrant, NA)

#Visualize using ggplot2
plot_map1 <- ggplot(data = merged_data) +
  geom_sf(aes(fill = significant_cluster)) +
  scale_fill_manual(values = c("High-High" = "red", "High-Low" = "orange", "Low-Low" = "blue", "Low-High" = "green")) +
  labs(title = "TB mortality LISA Cluster Map", subtitle = "Based on Residing District", fill = "Cluster Type") +
  theme_minimal()+
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(), # Remove axis text
    plot.title = element_text(size = 16, face = "bold"),  # Title styling  
    axis.ticks = element_blank(),   # Remove axis ticks
    panel.grid = element_blank()
  )
ggsave(
  filename = "C:/Users/MDMC (Workshop)/Documents/final_maharashtra/LISA(R)M_22.png",
  plot = plot_map1,  # Replace with your ggplot object
  width = 10, height = 8,   # Width and height in inches
  dpi = 1000                 # Resolution in dots per inch
)



merged_data <- merged_data11 %>% filter(Year == 2023)

#merged_data <- merge(MH_sp,data, by="District")

# Create neighbor list based on spatial contiguity (using queen criterion)
neighbors <- poly2nb(merged_data)

# Create spatial weights matrix from neighbors
weights <- nb2listw(neighbors, style = "W", zero.policy = TRUE)

# Run Moran's I test for global spatial autocorrelation
global_moran <- moran.test(merged_data$mortality_rate, weights, zero.policy = TRUE)

# Run Local Moran's I (LISA)
local_moran <- localmoran(merged_data$mortality_rate, weights, zero.policy = TRUE)

# Append LISA results to the merged data
merged_data$local_moran_I <- local_moran[, 1]  # Local Moran's I values
merged_data$p_value <- local_moran[, 5]  # P-values of the local test

# Quadrants for Local Moran's I
merged_data$quadrant <- NA
mean_cases <- mean(merged_data$mortality_rate, na.rm = TRUE)

for (i in 1:nrow(merged_data)) {
  if (merged_data$mortality_rate[i] > mean_cases & local_moran[i, 1] > 0) {
    merged_data$quadrant[i] <- "High-High"
  } else if (merged_data$mortality_rate[i] > mean_cases & local_moran[i, 1] < 0) {
    merged_data$quadrant[i] <- "High-Low"
  } else if (merged_data$mortality_rate[i] < mean_cases & local_moran[i, 1] > 0) {
    merged_data$quadrant[i] <- "Low-Low"
  } else if (merged_data$mortality_rate[i] < mean_cases & local_moran[i, 1] < 0) {
    merged_data$quadrant[i] <- "Low-High"
  }
}
# Filter for significant p-values
merged_data$significant_cluster <- ifelse(merged_data$p_value < 0.05, merged_data$quadrant, NA)

#Visualize using ggplot2
plot_map1 <- ggplot(data = merged_data) +
  geom_sf(aes(fill = significant_cluster)) +
  scale_fill_manual(values = c("High-High" = "red", "High-Low" = "orange", "Low-Low" = "blue", "Low-High" = "green")) +
  labs(title = "TB mortality LISA Cluster Map", subtitle = "Based on Residing District", fill = "Cluster Type") +
  theme_minimal()+
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(), # Remove axis text
    plot.title = element_text(size = 16, face = "bold"),  # Title styling  
    axis.ticks = element_blank(),   # Remove axis ticks
    panel.grid = element_blank()
  )
ggsave(
  filename = "C:/Users/MDMC (Workshop)/Documents/final_maharashtra/LISA(R)M_23.png",
  plot = plot_map1,  # Replace with your ggplot object
  width = 10, height = 8,   # Width and height in inches
  dpi = 1000                 # Resolution in dots per inch
)
