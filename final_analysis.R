data <- read.csv("C:/Users/MDMC (Workshop)/Documents/Bihar/Notification_Register/Diagnosed/Notification_Data/Bihar_merged_notification.csv")
data  <- subset(data, Spectrum_Current_State == "Bihar") ##Filter out the data from Bihar
INDIA <- readRDS('gadm36_IND_2_sf.rds') #shapefile 
BH_sp  <- subset(INDIA, NAME_1=="Bihar") ##Filter out the data from Bihar
pop <- read_excel("C:/Users/MDMC (Workshop)/Documents/population_bihar.xlsx")


N1=length(unique(data$Spectrum_Current_District)) ##number of districts before cleaning
data$Spectrum_Current_District <- gsub("ROHTAS", "Rohtas", data$Spectrum_Current_District)
data$Spectrum_Current_District <- gsub("MUNGER", "Munger", data$Spectrum_Current_District)
data$Spectrum_Current_District <- gsub("BHOJPUR", "Bhojpur", data$Spectrum_Current_District)
data$Spectrum_Current_District <- gsub("SUPAUL", "Supaul", data$Spectrum_Current_District)
data$Spectrum_Current_District <- gsub("BHAGALPUR", "Bhagalpur", data$Spectrum_Current_District)
data$Spectrum_Current_District <- gsub("NALANDA", "Nalanda", data$Spectrum_Current_District)
data$Spectrum_Current_District <- gsub("AURANGABAD-BI", "Aurangabad", data$Spectrum_Current_District)
data$Spectrum_Current_District <- gsub("Aurangabad-BI", "Aurangabad", data$Spectrum_Current_District)
data$Spectrum_Current_District <- gsub("PURNIA", "Purnia", data$Spectrum_Current_District)
data$Spectrum_Current_District <- gsub("ARARIA", "Araria", data$Spectrum_Current_District)
data$Spectrum_Current_District <- gsub("KATIHAR", "Katihar", data$Spectrum_Current_District)
data$Spectrum_Current_District <- gsub("BEGUSARAI", "Begusarai", data$Spectrum_Current_District)
data$Spectrum_Current_District <- gsub("SHEOHAR", "Sheohar", data$Spectrum_Current_District)
N1=length(unique(data$Spectrum_Current_District)) ##number of districts before cleaning (~38)

####Ensure same column name for districts in both the files####
names(BH_sp)[names(BH_sp) == "NAME_2"] <- "District"
names(data)[names(data) == "Spectrum_Current_District"] <- "District"

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
    total_cases = n(),
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
  mutate(incidence_rate = (total_cases / Population) * 100000) %>%
  dplyr::select(District, Year, incidence_rate)

district_map <- BH_sp %>%
  left_join(tb_incidence, by = "District")

####Extract the coordinates for each district in Maharashtra####
centroids <- st_centroid(BH_sp[1]$geometry)
centroids_coords <- st_coordinates(centroids)

library(ggplot2)
ggplot(district_map %>% filter(Year == 2024)) +
  geom_sf(aes(fill = incidence_rate)) +
  geom_text(aes(x = centroids_coords[,1], y = centroids_coords[,2], label = BH_sp$District), size = 2.3)+
  scale_fill_viridis_c(option = "turbo", name = "Incidence Rate", limits = c(45, 600)  # Narrow the range to focus on small changes
  ) +
  labs(title = "TB Incidence Rate by District", subtitle = "Year: 2022") +
  theme_minimal()




district_map$Year <- as.factor(district_map$Year)
#install.packages("gganimate")
library(gganimate)



animated_map <- ggplot(district_map) +
  geom_sf(aes(fill = incidence_rate)) +
  geom_text(aes(x = centroids_coords[,1], y = centroids_coords[,2], label = BH_sp$District), size = 2.3)+
  scale_fill_viridis_c(option = "turbo", name = "Incidence Rate", limits = c(45, 600)) +
  labs(title = "TB Incidence Rate by District", subtitle = "Year: {closest_state}") +
  theme_minimal() +
  transition_states(Year, transition_length = 2,state_length = 60) +
  ease_aes('cubic-in-out')
animate(animated_map, width=800,height=700, renderer=gifski_renderer())
anim_save("TB_incidence_rate__Bihar_animation.gif")
