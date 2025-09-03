# lecture 2
library(tidycensus)
library(sf)

tsunami_data <- st_read("Tsunami_Evacuation_Zones.geojson")

head(tsunami_data)

yes_zones <- tsunami_data[tsunami_data$evac_zone == "Yes", ]

pop_data <- get_acs(geography = "tract", state = "HI", variables = "B01003_001", geometry = TRUE, year = 2021)

pop_data <- st_transform(pop_data, crs = 4326)


intersected <- st_intersection(yes_zones, pop_data)
head(intersected)
total <- sum(intersected$estimate)
print(total)