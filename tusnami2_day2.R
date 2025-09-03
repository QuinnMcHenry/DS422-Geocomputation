#https://geoportal.hawaii.gov/datasets/tsunami-evacuation-zones/explore?showTable=true

library(tidyverse)
library(sf)
library(here)
library(mapgl)
library(tidycensus)
#library(rsocrata)

evac_zones <- st_read(here("Tsunami_Evacuation_Zones.geojson"))



# 2) Simple style (semi-transparent fill + outline)
fill_col <- "#ef4444"   # red

mapboxgl(bounds = evac_zones) |>
  add_fill_layer(
    id = "tsunami_evac",
    source = evac_zones,
    fill_color = fill_col,
    fill_opacity = 0.55,
    tooltip = "mapname"
  ) |>

  add_line_layer(
    id = "tsunami_layer",
    source = evac_zones,
    line_color = fill_col
  )|>
  add_legend(
    legend_title = "Tsunami Evacuation Zones",
    values = "Evacuation Area",
    colors = fill_col,
    type = "categorical"
  )




# Question 1: How many hawaii residents live in evacuation zones

# --- 1) Get ACS population with geometry (block groups) ---
bg <- get_acs(
  geography = "block group",
  variables = "B01003_001",   # total population
  state = "HI",
  year = 2023, survey = "acs5",
  geometry = TRUE, cache_table = TRUE
) |>
  st_transform(4326) |>
  st_make_valid() |>
  rename(pop = estimate) |>
  select(GEOID, pop)



block_groups <- bg |>
  mutate(blockgroup_area = st_area(geometry))

evac_zones <- evac_zones |>
  st_transform(4326) |>
  st_make_valid() |>
  mutate(evac_id = dplyr::row_number())



evac_zones <- evac_zones |>
  select(evac_id, island, zone_type, zone_desc)


intersections <- st_intersection(block_groups, evac_zones)


intersections <- intersections |>
  mutate(overlap_area = st_area(geometry))


intersections <- intersections |>
  mutate(weight = as.numeric(overlap_area / blockgroup_area))


intersections <- intersections |>
  mutate(pop_in_zone = pop * weight)


sum(intersections$pop_in_zone)







## Challenge 1 - Using the Tsunami_Evacuation_All_Zones.geojson (https://geoportal.hawaii.gov/datasets/437b77f132ed416294b45bde6aef23f4_11/explore?location=20.546870%2C-157.491600%2C7.83) can you tell me how many people are in Tsunami Evacuation Zones vs Extreme Tsunami Evacuation Zones
evac_all_zones <- st_read(here("Tsunami_Evacuation_-_All_Zones.geojson"))

evac_all_zones <- evac_all_zones |>
  st_transform(4326) |>
  st_make_valid() |>
  mutate(evac_id = dplyr::row_number())

extreme_zones <- evac_all_zones[(evac_all_zones$zone_type %in% "Extreme Tsunami Evacuation Zone"),]

bg <- get_acs(
  geography = "block group",
  variables = "B01003_001",   # total population
  state = "HI",
  year = 2023, survey = "acs5",
  geometry = TRUE, cache_table = TRUE
) |>
  st_transform(4326) |>
  st_make_valid() |>
  rename(pop = estimate) |>
  select(GEOID, pop)



block_groups <- bg |>
  mutate(blockgroup_area = st_area(geometry))


intersections <- st_intersection(block_groups, extreme_zones)


intersections <- intersections |>
  mutate(overlap_area = st_area(geometry))


intersections <- intersections |>
  mutate(weight = as.numeric(overlap_area / blockgroup_area))


intersections <- intersections |>
  mutate(pop_in_zone = pop * weight)


sum(intersections$pop_in_zone)

# 174,554.5 - no way


## Challenge 2 - What is the key assumption our analysis makes? Is this acceptable? How can it be improved?

"We are weighting the populations based on how much of the area overlaps with the evac area (intersections <- intersections |>
  mutate(weight = as.numeric(overlap_area / blockgroup_area))) ... which means we are 
assuming that the populations are even across the whole area, which is definitely not accurate. It's probably not super super
inaccurate as most of the time people will be relatively spread out - but hypothetically you could have a 
place that is like 90% extreme zone and 10% regular evac zone, and all of the population
lives in the 10%, and our code will classify 90% of the population as living in an extreme evac zone even though it is 0%. 
Maybe that is part of why the result of the code above is too high, but it definitely shouldn't be higher than the 
original evac zone number anyway."


"This could be improved by taking actual measurements of the disbursement of people by area. You could do this with satellite imagery
and use the rooftops to see where the most people actually live, maybe with machine learning. Or you could have a googlemaps type of 
thing and have a car drive around with a rendering thing on the roof, but either way to make this accurate you'd need granular case-by-case
data for the actual disbursement of population in each zone."



