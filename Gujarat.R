library(readxl)
library(dplyr)
library(tidyr)
library(sf)

data <- read.csv("C:/Users/MDMC (Workshop)/Documents/Gujarat_Notification_Cleaned_Imputed_Final.csv")
data  <- subset(data, Spectrum_Diagnosing_State == "Gujarat") ##Filter out the data from gujarat
INDIA <- readRDS('C:/Users/MDMC (Workshop)/Documents/gadm36_IND_2_sf.rds') #shapefile 
GJ_sp  <- subset(INDIA, NAME_1=="Gujarat") ##Filter out the data from gujarat
pop <- read_excel("C:/Users/MDMC (Workshop)/Documents/population_gujarat.xlsx")
#check <- subset(INDIA, NAME_1=="NCT of Delhi")

N1=length(unique(data$Spectrum_Diagnosing_District)) ##number of districts before cleaning
data$Spectrum_Diagnosing_District <- gsub("Panchmahals", "Panch Mahals", data$Spectrum_Diagnosing_District)
data$Spectrum_Diagnosing_District <- gsub("Surat Municipal Corporation", "Surat", data$Spectrum_Diagnosing_District)
data$Spectrum_Diagnosing_District <- gsub("Ahmadabad R", "Ahmedabad", data$Spectrum_Diagnosing_District)
data$Spectrum_Diagnosing_District <- gsub("AHMADABAD R", "Ahmedabad", data$Spectrum_Diagnosing_District)
data$Spectrum_Diagnosing_District <- gsub("Ahmedabad Municipal Corporation", "Ahmedabad", data$Spectrum_Diagnosing_District)
data$Spectrum_Diagnosing_District <- gsub("Surat Rural", "Surat", data$Spectrum_Diagnosing_District)
data$Spectrum_Diagnosing_District <- gsub("Banaskantha", "Banas Kantha", data$Spectrum_Diagnosing_District)
data$Spectrum_Diagnosing_District <- gsub("Vadodara R", "Vadodara", data$Spectrum_Diagnosing_District)
data$Spectrum_Diagnosing_District <- gsub("Jamnagar-Rural", "Jamnagar", data$Spectrum_Diagnosing_District)
data$Spectrum_Diagnosing_District <- gsub("Vyara", "Tapi", data$Spectrum_Diagnosing_District)
data$Spectrum_Diagnosing_District <- gsub("Vadodara Municipal Corporation", "Vadodara", data$Spectrum_Diagnosing_District)
data$Spectrum_Diagnosing_District <- gsub("Sabarkantha", "Sabar Kantha", data$Spectrum_Diagnosing_District)
data$Spectrum_Diagnosing_District <- gsub("The Dangs", "Dang", data$Spectrum_Diagnosing_District)
data$Spectrum_Diagnosing_District <- gsub("Chhotaudepur", "Chhota Udaipur", data$Spectrum_Diagnosing_District)
data$Spectrum_Diagnosing_District <- gsub("KHEDA", "Kheda", data$Spectrum_Diagnosing_District)
data$Spectrum_Diagnosing_District <- gsub("Devbhumi dwarka", "Devbhumi Dwarka", data$Spectrum_Diagnosing_District)
data$Spectrum_Diagnosing_District <- gsub("Jamnagar Municipal Corporation", "Jamnagar", data$Spectrum_Diagnosing_District)
data$Spectrum_Diagnosing_District <- gsub("Junagadh Municipal Corporation", "Junagadh", data$Spectrum_Diagnosing_District)
data$Spectrum_Diagnosing_District <- gsub("Rajkot Municipal Corporation", "Rajkot", data$Spectrum_Diagnosing_District)
data$Spectrum_Diagnosing_District <- gsub("ANAND", "Anand", data$Spectrum_Diagnosing_District)
data$Spectrum_Diagnosing_District <- gsub("Gandhinagar Municipal Corporation", "Gandhinagar", data$Spectrum_Diagnosing_District)
data$Spectrum_Diagnosing_District <- gsub("DEVBHUMI DWARKA", "Devbhumi Dwarka", data$Spectrum_Diagnosing_District)
data$Spectrum_Diagnosing_District <- gsub("PATAN", "Patan", data$Spectrum_Diagnosing_District)
data$Spectrum_Diagnosing_District <- gsub("BHAVNAGAR", "Bhavnagar", data$Spectrum_Diagnosing_District)
data$Spectrum_Diagnosing_District <- gsub("BANASKANTHA", "Banas Kantha", data$Spectrum_Diagnosing_District)
data$Spectrum_Diagnosing_District <- gsub("Bhavnagar Municipal Corporation", "Bhavnagar", data$Spectrum_Diagnosing_District)
data$Spectrum_Diagnosing_District <- gsub("BOTAD", "Botad", data$Spectrum_Diagnosing_District)
data$Spectrum_Diagnosing_District <- gsub("CHHOTAUDEPUR", "Chhota Udaipur", data$Spectrum_Diagnosing_District)
data$Spectrum_Diagnosing_District <- gsub("RAJKOT", "Rajkot", data$Spectrum_Diagnosing_District)
data$Spectrum_Diagnosing_District <- gsub("AMRELI", "Amreli", data$Spectrum_Diagnosing_District)
data$Spectrum_Diagnosing_District <- gsub("VADODARA R", "Vadodara", data$Spectrum_Diagnosing_District)
data$Spectrum_Diagnosing_District <- gsub("SABARKANTHA", "Sabar Kantha", data$Spectrum_Diagnosing_District)
data$Spectrum_Diagnosing_District <- gsub("Arvalli", "Aravalli", data$Spectrum_Diagnosing_District)
GJ_sp$NAME_2 <- gsub("Ahmadabad","Ahmedabad",GJ_sp$NAME_2)
#GJ_sp$NAME_2 <- gsub("Aravalli","Arvalli",GJ_sp$NAME_2)
GJ_sp$NAME_2 <- gsub("The Dangs","Dang",GJ_sp$NAME_2)
N1=length(unique(data$Spectrum_Diagnosing_District)) ##number of districts before cleaning (~38)

####Ensure same column name for districts in both the files####
names(GJ_sp)[names(GJ_sp) == "NAME_2"] <- "District"
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

district_map <- GJ_sp %>%
  left_join(tb_summary, by = "District")

district_map <- district_map %>%
  mutate(mortality_rate_frac = round(mortality_rate_frac, 2))

# Create the label column for district names with cases
district_map <- district_map %>%
  mutate(CFR = paste0(District, " (", mortality_rate_frac, ")"))

####Extract the coordinates for each district in Maharashtra####
centroids <- st_centroid(GJ_sp[1]$geometry)
centroids_coords <- st_coordinates(centroids)

district_map <- district_map %>% filter(Year != 2024)

library(ggplot2)

#incidence choropleth plot
plot_map <- district_map %>% filter(Year == 2021)
plot_map <- ggplot(plot_map) +
  geom_sf(aes(fill = incidence_rate), color = "black") +
  geom_sf_text(aes(label = GJ_sp$District), fontface = "bold", size = 3.0)+
  scale_fill_gradient(low = "yellow", high = "red", name = "Incidence rate", limits = c(62,1300), oob = scales::squish) +
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
  filename = "C:/Users/MDMC (Workshop)/Documents/final_gujarat/incidence_21.png",
  plot = plot_map,  # Replace with your ggplot object
  width = 10, height = 8,   # Width and height in inches
  dpi = 1000                 # Resolution in dots per inch
)

#incidence choropleth plot
plot_map <- district_map %>% filter(Year == 2022)
plot_map <- ggplot(plot_map) +
  geom_sf(aes(fill = incidence_rate), color = "black") +
  geom_sf_text(aes(label = GJ_sp$District), fontface = "bold", size = 3.0)+
  scale_fill_gradient(low = "yellow", high = "red", name = "Incidence rate", limits = c(62, 1300), oob = scales::squish) +
  labs(title = "TB Incidence Rate per lakh across District", subtitle = "Year: 2022") +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(), # Remove axis text
    plot.title = element_text(size = 16, face = "bold"),  # Title styling  
    axis.ticks = element_blank(),   # Remove axis ticks
    panel.grid = element_blank()
  )
ggsave(
  filename = "C:/Users/MDMC (Workshop)/Documents/final_gujarat/incidence_22.png",
  plot = plot_map,  # Replace with your ggplot object
  width = 10, height = 8,   # Width and height in inches
  dpi = 1000                 # Resolution in dots per inch
)

#incidence choropleth plot
plot_map <- district_map %>% filter(Year == 2023)
plot_map <- ggplot(plot_map) +
  geom_sf(aes(fill = incidence_rate), color = "black") +
  geom_sf_text(aes(label = GJ_sp$District), fontface = "bold", size = 3.0)+
  scale_fill_gradient(low = "yellow", high = "red", name = "Incidence rate", limits = c(62, 1300), oob = scales::squish) +
  labs(title = "TB Incidence Rate per lakh across District", subtitle = "Year: 2023") +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(), # Remove axis text
    plot.title = element_text(size = 16, face = "bold"),  # Title styling  
    axis.ticks = element_blank(),   # Remove axis ticks
    panel.grid = element_blank()
  )
ggsave(
  filename = "C:/Users/MDMC (Workshop)/Documents/final_gujarat/incidence_23.png",
  plot = plot_map,  # Replace with your ggplot object
  width = 10, height = 8,   # Width and height in inches
  dpi = 1000                 # Resolution in dots per inch
)



#mortality choropleth plot
plot_map <- district_map %>% filter(Year == 2021)
plot_map <- ggplot(plot_map) +
  geom_sf(aes(fill = mortality_rate), color = "black") +
  geom_sf_text(aes(label = GJ_sp$District), fontface = "bold", size = 3.0)+
  scale_fill_gradient(low = "white", high = "red", name = "Mortality rate", limits = c(3.5,102)) +
  labs(title = "TB Mortality Rate per lakh across District", subtitle = "Year: 2021") +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(), # Remove axis text
    plot.title = element_text(size = 16, face = "bold"),  # Title styling  
    axis.ticks = element_blank(),   # Remove axis ticks
    panel.grid = element_blank()
  )
ggsave(
  filename = "C:/Users/MDMC (Workshop)/Documents/final_gujarat/mortality_21.png",
  plot = plot_map,  # Replace with your ggplot object
  width = 10, height = 8,   # Width and height in inches
  dpi = 1000                # Resolution in dots per inch
)

#mortality choropleth plot
plot_map <- district_map %>% filter(Year == 2022)
plot_map <- ggplot(plot_map) +
  geom_sf(aes(fill = mortality_rate), color = "black") +
  geom_sf_text(aes(label = GJ_sp$District), fontface = "bold", size = 3.0)+
  scale_fill_gradient(low = "white", high = "red", name = "Mortality rate", limits = c(3.5,102)) +
  labs(title = "TB Mortality Rate per lakh across District", subtitle = "Year: 2022") +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(), # Remove axis text
    plot.title = element_text(size = 16, face = "bold"),  # Title styling  
    axis.ticks = element_blank(),   # Remove axis ticks
    panel.grid = element_blank()
  )
ggsave(
  filename = "C:/Users/MDMC (Workshop)/Documents/final_gujarat/mortality_22.png",
  plot = plot_map,  # Replace with your ggplot object
  width = 10, height = 8,   # Width and height in inches
  dpi = 1000                # Resolution in dots per inch
)

#mortality choropleth plot
plot_map <- district_map %>% filter(Year == 2023)
plot_map <- ggplot(plot_map) +
  geom_sf(aes(fill = mortality_rate), color = "black") +
  geom_sf_text(aes(label = GJ_sp$District), fontface = "bold", size = 3.0)+
  scale_fill_gradient(low = "white", high = "red", name = "Mortality rate", limits = c(3.5,102)) +
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
  filename = "C:/Users/MDMC (Workshop)/Documents/final_gujarat/mortality_23.png",
  plot = plot_map,  # Replace with your ggplot object
  width = 10, height = 8,   # Width and height in inches
  dpi = 1000                # Resolution in dots per inch
)


#CFR choropleth plot
plot_map <- district_map %>% filter(Year == 2021)
plot_map1 <- ggplot(plot_map) +
  geom_sf(aes(fill = mortality_rate_frac), color = "black") +
  geom_sf_text(aes(label = plot_map$CFR), fontface = "bold", size = 3.0)+
  scale_fill_gradient(low = "white", high = "red", name = "CFR (%)", limits = c(3.2,9.6)) +
  labs(title = "TB-CFR across District", subtitle = "Year: 2021") +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(), # Remove axis text
    plot.title = element_text(size = 16, face = "bold"),  # Title styling  
    axis.ticks = element_blank(),   # Remove axis ticks
    panel.grid = element_blank()
  )
ggsave(
  filename = "C:/Users/MDMC (Workshop)/Documents/final_gujarat/CFR_21.png",
  plot = plot_map1,  # Replace with your ggplot object
  width = 10, height = 8,   # Width and height in inches
  dpi = 1000                 # Resolution in dots per inch
)


#CFR choropleth plot
plot_map <- district_map %>% filter(Year == 2022)
plot_map1 <- ggplot(plot_map) +
  geom_sf(aes(fill = mortality_rate_frac), color = "black") +
  geom_sf_text(aes(label = plot_map$CFR), fontface = "bold", size = 3.0)+
  scale_fill_gradient(low = "white", high = "red", name = "CFR (%)", limits = c(3.2,9.6)) +
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
  filename = "C:/Users/MDMC (Workshop)/Documents/final_gujarat/CFR_22.png",
  plot = plot_map1,  # Replace with your ggplot object
  width = 10, height = 8,   # Width and height in inches
  dpi = 1000                 # Resolution in dots per inch
)


#CFR choropleth plot
plot_map <- district_map %>% filter(Year == 2023)
plot_map1 <- ggplot(plot_map) +
  geom_sf(aes(fill = mortality_rate_frac), color = "black") +
  geom_sf_text(aes(label = plot_map$CFR), fontface = "bold", size = 3.0)+
  scale_fill_gradient(low = "white", high = "red", name = "CFR (%)", limits = c(3.2,9.6)) +
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
  filename = "C:/Users/MDMC (Workshop)/Documents/final_gujarat/CFR_23.png",
  plot = plot_map1,  # Replace with your ggplot object
  width = 10, height = 8,   # Width and height in inches
  dpi = 1000                 # Resolution in dots per inch
)



merged_data <- district_map %>% filter(Year == 2021)

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
  filename = "C:/Users/MDMC (Workshop)/Documents/final_gujarat/LISA(D)_21.png",
  plot = plot_map1,  # Replace with your ggplot object
  width = 10, height = 8,   # Width and height in inches
  dpi = 1000                 # Resolution in dots per inch
)




merged_data <- district_map %>% filter(Year == 2022)

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
  filename = "C:/Users/MDMC (Workshop)/Documents/final_gujarat/LISA(D)_22.png",
  plot = plot_map1,  # Replace with your ggplot object
  width = 10, height = 8,   # Width and height in inches
  dpi = 1000                 # Resolution in dots per inch
)


merged_data <- district_map %>% filter(Year == 2023)

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
  filename = "C:/Users/MDMC (Workshop)/Documents/final_gujarat/LISA(D)_23.png",
  plot = plot_map1,  # Replace with your ggplot object
  width = 10, height = 8,   # Width and height in inches
  dpi = 1000                 # Resolution in dots per inch
)







merged_data <- district_map %>% filter(Year == 2021)

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
  filename = "C:/Users/MDMC (Workshop)/Documents/final_gujarat/LISA(D)M_21.png",
  plot = plot_map1,  # Replace with your ggplot object
  width = 10, height = 8,   # Width and height in inches
  dpi = 1000                 # Resolution in dots per inch
)




merged_data <- district_map %>% filter(Year == 2022)

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
  filename = "C:/Users/MDMC (Workshop)/Documents/final_gujarat/LISA(D)M_22.png",
  plot = plot_map1,  # Replace with your ggplot object
  width = 10, height = 8,   # Width and height in inches
  dpi = 1000                 # Resolution in dots per inch
)


merged_data <- district_map %>% filter(Year == 2023)

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
  filename = "C:/Users/MDMC (Workshop)/Documents/final_gujarat/LISA(D)M_23.png",
  plot = plot_map1,  # Replace with your ggplot object
  width = 10, height = 8,   # Width and height in inches
  dpi = 1000                 # Resolution in dots per inch
)



##################################################################################################

data <- read.csv("C:/Users/MDMC (Workshop)/Documents/Gujarat_Notification_Cleaned_Imputed_Final.csv")
data  <- subset(data, Spectrum_Residence_State == "Gujarat") ##Filter out the data from gujarat
INDIA <- readRDS('C:/Users/MDMC (Workshop)/Documents/gadm36_IND_2_sf.rds') #shapefile 
GJ_sp  <- subset(INDIA, NAME_1=="Gujarat") ##Filter out the data from gujarat
pop <- read_excel("C:/Users/MDMC (Workshop)/Documents/population_gujarat.xlsx")


N1=length(unique(data$Spectrum_Residence_District)) ##number of districts before cleaning
data$Spectrum_Residence_District <- gsub("Panchmahals", "Panch Mahals", data$Spectrum_Residence_District)
data$Spectrum_Residence_District <- gsub("Surat Municipal Corporation", "Surat", data$Spectrum_Residence_District)
data$Spectrum_Residence_District <- gsub("Ahmadabad R", "Ahmedabad", data$Spectrum_Residence_District)
data$Spectrum_Residence_District <- gsub("AHMADABAD R", "Ahmedabad", data$Spectrum_Residence_District)
data$Spectrum_Residence_District <- gsub("Ahmedabad Municipal Corporation", "Ahmedabad", data$Spectrum_Residence_District)
data$Spectrum_Residence_District <- gsub("Surat Rural", "Surat", data$Spectrum_Residence_District)
data$Spectrum_Residence_District <- gsub("Banaskantha", "Banas Kantha", data$Spectrum_Residence_District)
data$Spectrum_Residence_District <- gsub("Vadodara R", "Vadodara", data$Spectrum_Residence_District)
data$Spectrum_Residence_District <- gsub("Jamnagar-Rural", "Jamnagar", data$Spectrum_Residence_District)
data$Spectrum_Residence_District <- gsub("Vyara", "Tapi", data$Spectrum_Residence_District)
data$Spectrum_Residence_District <- gsub("Vadodara Municipal Corporation", "Vadodara", data$Spectrum_Residence_District)
data$Spectrum_Residence_District <- gsub("Sabarkantha", "Sabar Kantha", data$Spectrum_Residence_District)
data$Spectrum_Residence_District <- gsub("The Dangs", "Dang", data$Spectrum_Residence_District)
data$Spectrum_Residence_District <- gsub("Chhotaudepur", "Chhota Udaipur", data$Spectrum_Residence_District)
data$Spectrum_Residence_District <- gsub("KHEDA", "Kheda", data$Spectrum_Residence_District)
data$Spectrum_Residence_District <- gsub("Devbhumi dwarka", "Devbhumi Dwarka", data$Spectrum_Residence_District)
data$Spectrum_Residence_District <- gsub("Jamnagar Municipal Corporation", "Jamnagar", data$Spectrum_Residence_District)
data$Spectrum_Residence_District <- gsub("Junagadh Municipal Corporation", "Junagadh", data$Spectrum_Residence_District)
data$Spectrum_Residence_District <- gsub("Rajkot Municipal Corporation", "Rajkot", data$Spectrum_Residence_District)
data$Spectrum_Residence_District <- gsub("ANAND", "Anand", data$Spectrum_Residence_District)
data$Spectrum_Residence_District <- gsub("Gandhinagar Municipal Corporation", "Gandhinagar", data$Spectrum_Residence_District)
data$Spectrum_Residence_District <- gsub("DEVBHUMI DWARKA", "Devbhumi Dwarka", data$Spectrum_Residence_District)
data$Spectrum_Residence_District <- gsub("PATAN", "Patan", data$Spectrum_Residence_District)
data$Spectrum_Residence_District <- gsub("BHAVNAGAR", "Bhavnagar", data$Spectrum_Residence_District)
data$Spectrum_Residence_District <- gsub("BANASKANTHA", "Banas Kantha", data$Spectrum_Residence_District)
data$Spectrum_Residence_District <- gsub("Bhavnagar Municipal Corporation", "Bhavnagar", data$Spectrum_Residence_District)
data$Spectrum_Residence_District <- gsub("BOTAD", "Botad", data$Spectrum_Residence_District)
data$Spectrum_Residence_District <- gsub("CHHOTAUDEPUR", "Chhota Udaipur", data$Spectrum_Residence_District)
data$Spectrum_Residence_District <- gsub("RAJKOT", "Rajkot", data$Spectrum_Residence_District)
data$Spectrum_Residence_District <- gsub("AMRELI", "Amreli", data$Spectrum_Residence_District)
data$Spectrum_Residence_District <- gsub("VADODARA R", "Vadodara", data$Spectrum_Residence_District)
data$Spectrum_Residence_District <- gsub("SABARKANTHA", "Sabar Kantha", data$Spectrum_Residence_District)
data$Spectrum_Residence_District <- gsub("Arvalli", "Aravalli", data$Spectrum_Residence_District)
GJ_sp$NAME_2 <- gsub("Ahmadabad","Ahmedabad",GJ_sp$NAME_2)
#GJ_sp$NAME_2 <- gsub("Aravalli","Arvalli",GJ_sp$NAME_2)
GJ_sp$NAME_2 <- gsub("The Dangs","Dang",GJ_sp$NAME_2)
N1=length(unique(data$Spectrum_Residence_District)) ##number of districts before cleaning (~38)

####Ensure same column name for districts in both the files####
names(GJ_sp)[names(GJ_sp) == "NAME_2"] <- "District"
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

district_map <- GJ_sp %>%
  left_join(tb_summary, by = "District")

district_map <- district_map %>%
  mutate(mortality_rate_frac = round(mortality_rate_frac, 2))

# Create the label column for district names with cases
district_map <- district_map %>%
  mutate(CFR = paste0(District, " (", mortality_rate_frac, ")"))

####Extract the coordinates for each district in Maharashtra####
centroids <- st_centroid(GJ_sp[1]$geometry)
centroids_coords <- st_coordinates(centroids)




merged_data <- district_map %>% filter(Year == 2021)

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
  labs(title = "TB incidence LISA Cluster Map", subtitle = "Based on Residing District", fill = "Cluster Type") +
  theme_minimal()+
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(), # Remove axis text
    plot.title = element_text(size = 16, face = "bold"),  # Title styling  
    axis.ticks = element_blank(),   # Remove axis ticks
    panel.grid = element_blank()
  )
ggsave(
  filename = "C:/Users/MDMC (Workshop)/Documents/final_gujarat/LISA(R)_21.png",
  plot = plot_map1,  # Replace with your ggplot object
  width = 10, height = 8,   # Width and height in inches
  dpi = 1000                 # Resolution in dots per inch
)




merged_data <- district_map %>% filter(Year == 2022)

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
  labs(title = "TB incidence LISA Cluster Map", subtitle = "Based on Residing District", fill = "Cluster Type") +
  theme_minimal()+
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(), # Remove axis text
    plot.title = element_text(size = 16, face = "bold"),  # Title styling  
    axis.ticks = element_blank(),   # Remove axis ticks
    panel.grid = element_blank()
  )
ggsave(
  filename = "C:/Users/MDMC (Workshop)/Documents/final_gujarat/LISA(R)_22.png",
  plot = plot_map1,  # Replace with your ggplot object
  width = 10, height = 8,   # Width and height in inches
  dpi = 1000                 # Resolution in dots per inch
)


merged_data <- district_map %>% filter(Year == 2023)

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
  labs(title = "TB incidence LISA Cluster Map", subtitle = "Based on Residing District", fill = "Cluster Type") +
  theme_minimal()+
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(), # Remove axis text
    plot.title = element_text(size = 16, face = "bold"),  # Title styling  
    axis.ticks = element_blank(),   # Remove axis ticks
    panel.grid = element_blank()
  )
ggsave(
  filename = "C:/Users/MDMC (Workshop)/Documents/final_gujarat/LISA(R)_23.png",
  plot = plot_map1,  # Replace with your ggplot object
  width = 10, height = 8,   # Width and height in inches
  dpi = 1000                 # Resolution in dots per inch
)



merged_data <- district_map %>% filter(Year == 2021)

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
  filename = "C:/Users/MDMC (Workshop)/Documents/final_gujarat/LISA(R)M_21.png",
  plot = plot_map1,  # Replace with your ggplot object
  width = 10, height = 8,   # Width and height in inches
  dpi = 1000                 # Resolution in dots per inch
)




merged_data <- district_map %>% filter(Year == 2022)

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
  filename = "C:/Users/MDMC (Workshop)/Documents/final_gujarat/LISA(R)M_22.png",
  plot = plot_map1,  # Replace with your ggplot object
  width = 10, height = 8,   # Width and height in inches
  dpi = 1000                 # Resolution in dots per inch
)


merged_data <- district_map %>% filter(Year == 2023)

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
  filename = "C:/Users/MDMC (Workshop)/Documents/final_gujarat/LISA(R)M_23.png",
  plot = plot_map1,  # Replace with your ggplot object
  width = 10, height = 8,   # Width and height in inches
  dpi = 1000                 # Resolution in dots per inch
)
















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
    incidence_rate = n(),
    Treated = sum(Treatment_Outcome == "CURED" | Treatment_Outcome == "TREATMENT_COMPLETE" , na.rm = TRUE),
    Diabetic_status = sum(Status_of_Diabetes == "Diabetic", na.rm = TRUE),
    Non_diabetic_status = sum(Status_of_Diabetes == "Non-diabetic", na.rm = TRUE),
    Reactive = sum(Status_of_HIV == "Reactive", na.rm = TRUE),
    Non_reactive = sum(Status_of_HIV == "Non-Reactive", na.rm = TRUE),
    Positive = sum(Status_of_HIV == "Positive", na.rm = TRUE)
  ) %>%
  arrange(District, Year)


tb_incidence <- summary %>%
  inner_join(population, by = c("District", "Year")) %>%
  mutate(incidence_rate = (incidence_rate / Population) * 100000) %>%
  mutate(mortality_rate = (Treated / Population) * 100000) %>%
  dplyr::select(District, Year, incidence_rate, mortality_rate)

district_map <- GJ_sp %>%
  left_join(tb_incidence, by = "District")

####Extract the coordinates for each district in Maharashtra####
centroids <- st_centroid(GJ_sp[1]$geometry)
centroids_coords <- st_coordinates(centroids)

merged_data <- district_map %>% filter(Year == 2023)

#merged_data <- merge(GJ_sp,data, by="District")

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
plot_map <- ggplot(data = merged_data) +
  geom_sf(aes(fill = significant_cluster)) +
  scale_fill_manual(values = c("High-High" = "red", "High-Low" = "orange", "Low-Low" = "blue", "Low-High" = "green")) +
  labs(title = "LISA Cluster Map",
       subtitle = "Year: 2023",
       fill = "Cluster Type") +
  theme_minimal()

ggsave(
  filename = "C:/Users/MDMC (Workshop)/Documents/final_gujarat/residence_23.jpeg",
  plot = plot_map,  # Replace with your ggplot object
  width = 10, height = 8,   # Width and height in inches
  dpi = 600                 # Resolution in dots per inch
)
