## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2023-03-25
##

library(tidyverse)
library(mapview)
library(sf)

sf_crime <- read_csv("created_data/sf_incidents_ungeocoded.csv")
census_block_sf <- st_read("data/sf_census_data.shp")

sf_crime_geo <- sf_crime %>% 
  drop_na(latitude) %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = st_crs(census_block_sf))

sf_joined <- st_join(sf_crime_geo, census_block_sf, join = st_within)

sf_joined <- sf_joined %>% 
  st_drop_geometry() %>% 
  mutate(incident_year = year(incident_date),
         incident_month = month(incident_date),
         incident_year_month = mdy(paste0(incident_month, "-1-", incident_year)))


monthly_crime_sf <- sf_joined %>% 
  group_by(incident_year_month,NAME,GEOID) %>% 
  summarize(number_crimes = n()) %>% 
  ungroup()

monthly_crime_sf %>% 
  write_csv("created_data/sf_incidents_geocoded_monthly.csv")
