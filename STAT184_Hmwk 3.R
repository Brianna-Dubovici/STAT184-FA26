# Homework 3
# Problem 1: Reproducing plots

# Load packages
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(sf)

# Load datasets
vehicles <- ggplot2::mpg
gems <- ggplot2::diamonds

#PROBLEM 1

#Part A
# Problem 1A

p_A <- ggplot(vehicles, aes(x = displ, y = hwy)) +
  geom_point(aes(color = drv)) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  facet_wrap(~ drv) +
  labs(
    x = "Engine displacement (litres)",
    y = "Highway fuel economy (mpg)",
    color = "drv"
  ) +
  theme_bw()

p_A

# There are three fitted lines because the data are separated into
# three panels using facet_wrap(~ drv). The geom_smooth() layer
# calculates a separate regression line for each drive type.
# Therefore, each panel has its own fitted line.

#Part B
# Problem 1B: Stacked bar chart

p_B <- ggplot(vehicles, aes(x = drv, fill = factor(cyl))) +
  geom_bar() +
  labs(
    x = "Drive type",
    y = "Number of vehicles",
    fill = "Number of cylinders"
  ) +
  theme_bw()

p_B

# Problem 1B: Proportion bar chart

p_B_prop <- ggplot(vehicles, aes(x = drv, fill = factor(cyl))) +
  geom_bar(position = "fill") +
  labs(
    x = "Drive type",
    y = "Proportion",
    fill = "Number of cylinders"
  ) +
  theme_bw()

p_B_prop

# Equal total heights in the proportion plot do not mean
# the drive types have equal sample sizes. Each bar is
# scaled to represent proportions rather than the actual
# number of vehicles. Drive types can have different
# sample sizes but still have bars with equal heights.

#Part C

#Step 1
# Problem 1C: Create smaller dataset

gems_small <- gems %>%
  filter(carat <= 2.0)

#Step 2:
# Count retained diamonds
nrow(gems_small)

# Count original diamonds
nrow(gems)

# Count excluded diamonds
nrow(gems) - nrow(gems_small)

#Step 3: 
# Problem 1C: Fine histogram

p_fine <- ggplot(gems_small, aes(x = carat)) +
  geom_histogram(
    binwidth = 0.01,
    boundary = 0,
    closed = "right"
  ) +
  coord_cartesian(xlim = c(0, 2)) +
  labs(
    title = "Diamond Carat Distribution",
    subtitle = "Bin width = 0.01",
    x = "Carat",
    y = "Count"
  ) +
  theme_bw()

#Step 4: 
p_fine

#Step 5: 
# Problem 1C: Coarse histogram

p_coarse <- ggplot(gems_small, aes(x = carat)) +
  geom_histogram(
    binwidth = 0.5,
    boundary = 0,
    closed = "right"
  ) +
  coord_cartesian(xlim = c(0, 2)) +
  labs(
    title = "Diamond Carat Distribution",
    subtitle = "Bin width = 0.5",
    x = "Carat",
    y = "Count"
  ) +
  theme_bw()

#Step 6: 
p_coarse

#Step 7: 
# Problem 1C: Exact carat counts

gems %>%
  filter(carat == 0.99 | carat == 1.00) %>%
  count(carat)

#Step 8: 
# Narrower bins make it easier to see small differences
# and patterns in the distribution of diamond carat values.
# Wider bins make the overall distribution easier to see
# but hide some smaller details.

#PROBLEM 2:

#Step 1: Import Chicago library data

libraries <- read_csv("Libraries_-_Locations,__Contact_Information,_and_Usual_Hours_of_Operation_20261108.csv")

# View the data
View(libraries)

# Check the column names
names(libraries)

library(tidyverse)
names(libraries)

#Step 2: 
libraries <- libraries %>%
  separate(
    LOCATION,
    into = c("latitude", "longitude"),
    sep = ",",
    remove = FALSE,
    extra = "merge",
    fill = "right"
  ) %>%
  mutate(
    latitude = as.numeric(str_trim(latitude)),
    longitude = as.numeric(str_trim(longitude))
  )

#Step 3: 
head(libraries[, c("BRANCH", "LOCATION", "latitude", "longitude")])

#Step 4: 
sum(is.na(libraries$latitude))
sum(is.na(libraries$longitude))
sum(
  libraries$latitude < -90 |
    libraries$latitude > 90 |
    libraries$longitude < -180 |
    libraries$longitude > 180,
  na.rm = TRUE
)

libraries <- libraries %>%
  mutate(
    LOCATION = str_remove_all(LOCATION, "[()]")
  ) %>%
  separate(
    LOCATION,
    into = c("latitude", "longitude"),
    sep = ",",
    remove = FALSE
  ) %>%
  mutate(
    latitude = as.numeric(str_trim(latitude)),
    longitude = as.numeric(str_trim(longitude))
  )

head(libraries[, c("BRANCH", "latitude", "longitude")])
sum(is.na(libraries$latitude))
sum(is.na(libraries$longitude))
summary(libraries$latitude)
summary(libraries$longitude)

#Step 5: 
nrow(libraries)
sum(duplicated(libraries$BRANCH))
sum(duplicated(libraries[, c("latitude", "longitude")]))

#Step 6: 
libraries_clean <- libraries %>%
  mutate(
    site_id = row_number()
  ) %>%
  select(
    site_id,
    BRANCH,
    latitude,
    longitude,
    everything()
  )

head(libraries_clean)

#Step 7: 
library(sf)

#Step 8: 
library(tidyverse)
library(sf)

sites_m <- st_as_sf(
  libraries_clean,
  coords = c("longitude", "latitude"),
  crs = 4326,
  remove = FALSE
) %>%
  st_transform(26916)

#Step 9: 
p_sites <- ggplot() +
  geom_sf(data = sites_m, color = "blue", size = 2) +
  labs(
    title = "Chicago Public Library Locations",
    subtitle = "All retained library locations",
    caption = "Source: City of Chicago library locations dataset"
  ) +
  theme_minimal()

p_sites

#Step 10A: 
buffers_500 <- st_buffer(
  sites_m,
  dist = 500
)

#Step 10B: 
buffer_500_union <- st_union(buffers_500)

#Step 10C: 
p_buffers <- ggplot() +
  geom_sf(
    data = buffer_500_union,
    fill = "lightblue",
    color = "blue",
    alpha = 0.5
  ) +
  geom_sf(
    data = sites_m,
    color = "red",
    size = 2
  ) +
  labs(
    title = "Areas Within 500 Meters of Chicago Public Libraries",
    subtitle = "Combined coverage area of all library locations",
    caption = "Source: City of Chicago library locations dataset",
    fill = "500-meter coverage",
    color = "Library locations"
  ) +
  theme_minimal()

p_buffers

#Step 11A: 
buffers_1000 <- st_buffer(
  sites_m,
  dist = 1000
)

#Step 11B: 
buffer_1000_union <- st_union(buffers_1000)

#Step 12: 
area_500_km2 <- as.numeric(st_area(buffer_500_union)) / 1000000

area_1000_km2 <- as.numeric(st_area(buffer_1000_union)) / 1000000

area_500_km2
area_1000_km2

#Step 13: 
buffer_summary <- tibble(
  radius_m = c(500, 1000),
  union_area_km2 = c(area_500_km2, area_1000_km2)
)

buffer_summary

#Step 14: 
area_1000_km2 >= area_500_km2

#Step 15: 
# Check number of library records
nrow(libraries)

# Check number of cleaned records
nrow(libraries_clean)

# Check missing coordinates
sum(is.na(libraries$latitude))
sum(is.na(libraries$longitude))

# Check duplicate branch names
sum(duplicated(libraries$BRANCH))

# Check duplicate coordinates
sum(duplicated(libraries[, c("latitude", "longitude")]))

#Step 17: 
p_sites <- ggplot() +
  geom_sf(
    data = sites_m,
    aes(color = "Library locations"),
    size = 2
  ) +
  scale_color_manual(
    name = "Map Legend",
    values = c("Library locations" = "blue")
  ) +
  labs(
    title = "Chicago Public Library Locations",
    subtitle = "All 82 retained library locations",
    caption = "Source: City of Chicago library locations dataset. Accessed November 8, 2026."
  ) +
  theme_minimal()
p_sites

#Step 18: 
p_buffers <- ggplot() +
  geom_sf(
    data = buffer_500_union,
    aes(fill = "500-meter coverage"),
    color = "blue",
    alpha = 0.5
  ) +
  geom_sf(
    data = sites_m,
    aes(color = "Library locations"),
    size = 2
  ) +
  scale_fill_manual(
    name = "Map Legend",
    values = c("500-meter coverage" = "lightblue")
  ) +
  scale_color_manual(
    name = "Map Legend",
    values = c("Library locations" = "red")
  ) +
  labs(
    title = "Areas Within 500 Meters of Chicago Public Libraries",
    subtitle = "Combined coverage area of all 82 library locations",
    caption = "Source: City of Chicago library locations dataset. Accessed November 8, 2026."
  ) +
  theme_minimal()
p_buffers

#PROBLEM 3: 

#Step 1: 
community_areas_raw <- st_read("community_areas.geojson")

#Step 2: 
names(community_areas_raw)

#Step 3: 
community_areas <- community_areas_raw %>%
  select(
    district_id = area_numbe,
    district_name = community,
    geometry
  ) %>%
  st_transform(26916)
st_crs(community_areas)

#Step 4: 
# Check number of community areas
nrow(community_areas)

# Check for missing IDs or names
sum(is.na(community_areas$district_id))
sum(is.na(community_areas$district_name))

# Check for duplicate IDs
sum(duplicated(community_areas$district_id))

# Check for empty geometries
sum(st_is_empty(community_areas))

#Step 5: 
community_areas <- community_areas %>%
  mutate(
    area_km2 = as.numeric(st_area(geometry)) / 1000000
  )
summary(community_areas$area_km2)

#Step 6: 
# Find which community areas each library intersects
matches <- st_intersects(sites_m, community_areas)

# Create an audit showing the number of matches for each library
site_match_audit <- tibble(
  site_id = sites_m$site_id,
  n_area_matches = lengths(matches)
)
table(site_match_audit$n_area_matches)

#Step 8: 
# Save the site matching audit
write_csv(
  site_match_audit,
  "site_match_audit.csv"
)
# Match each library to its community area
site_assignments <- tibble(
  site_id = sites_m$site_id,
  district_id = community_areas$district_id[
    vapply(matches, function(x) x[1], integer(1))
  ]
)
# Count libraries in each community area
library_counts <- site_assignments %>%
  count(district_id, name = "n_sites")
district_counts <- community_areas %>%
  st_drop_geometry() %>%
  left_join(library_counts, by = "district_id") %>%
  mutate(
    n_sites = replace_na(n_sites, 0L),
    sites_per_km2 = n_sites / area_km2
  ) %>%
  select(
    district_id,
    district_name,
    n_sites,
    area_km2,
    sites_per_km2
  )
# Check for invalid geometries
sum(!st_is_valid(community_areas))
district_counts
sum(district_counts$n_sites)
nrow(district_counts)

#Step 9: Rank community areas by number of libraries
district_counts %>%
  arrange(desc(n_sites)) %>%
  select(district_name, n_sites) %>%
  head(10)

#Step 10: Rank community areas by libraries per square kilometer
district_counts %>%
  arrange(desc(sites_per_km2)) %>%
  select(district_name, n_sites, area_km2, sites_per_km2) %>%
  head(10)

#Step 11: 
# Combine community area boundaries with library counts
community_areas_map <- community_areas %>%
  left_join(
    district_counts,
    by = c("district_id", "district_name", "area_km2")
  )

#Step 12: 
p_count <- ggplot() +
  geom_sf(
    data = community_areas_map,
    aes(fill = n_sites),
    color = "white",
    linewidth = 0.2
  ) +
  scale_fill_viridis_c(
    name = "Number of libraries"
  ) +
  labs(
    title = "Number of Chicago Public Libraries by Community Area",
    subtitle = "Total libraries located in each community area",
    caption = "Source: City of Chicago Community Area Boundaries and Chicago Public Library Locations. Accessed November 8, 2026. All 82 libraries assigned to exactly one community area."
  ) +
  theme_minimal()
  
  head(10)
  p_count
  
#Step 13: 
  p_density <- ggplot() +
    geom_sf(
      data = community_areas_map,
      aes(fill = sites_per_km2),
      color = "white",
      linewidth = 0.2
    ) +
    scale_fill_viridis_c(
      name = "Libraries per km²"
    ) +
    labs(
      title = "Chicago Public Library Density by Community Area",
      subtitle = "Number of libraries per square kilometer",
      caption = "Source: City of Chicago Community Area Boundaries and Chicago Public Library Locations. Accessed November 8, 2026. All 82 libraries assigned to exactly one community area."
    ) +
    theme_minimal()
  p_density
  
#Step 14: 
  # Save the community area results
  write_csv(
    district_counts,
    "district_counts.csv"
  )
  sum(district_counts$n_sites)
  nrow(district_counts)
  sum(district_counts$n_sites == 0)
  
#Step 15: 
  data_sources <- tibble(
    dataset = c(
      "Libraries - Locations, Contact Information, and Usual Hours of Operation",
      "Boundaries - Community Areas"
    ),
    source = c(
      "City of Chicago Data Portal",
      "City of Chicago Data Portal"
    ),
    url = c(
      "https://data.cityofchicago.org/d/x8fc-8rcq",
      "https://data.cityofchicago.org/d/igwz-8jzy"
    ),
    access_date = c(
      "2026-11-08",
      "2026-11-08"
    )
  )
  
  write_csv(data_sources, "data_sources.csv")

#Check: 
  libraries <- read_csv("Libraries_-_Locations,__Contact_Information,_and_Usual_Hours_of_Operation_20261108.csv")
  
  community_areas_raw <- st_read("community_areas.geojson")
  
