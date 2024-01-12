## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2023-03-07
##

library(tidyverse)
library(mapview)


# loading in data ---------------------------------------------------------

address <- read_csv("data/all_addresses_with_coords.csv")

census_block_sf <- st_read("data/sf_census_data.shp")

census_block_sd <- st_read("data/sd_census_data.shp")

# merging addresses to census blocks ------------------------------------

## first changing to the correct coordinate reference system
address <- address %>% 
  st_as_sf(coords = c("x", "y"), crs = st_crs(census_block_sf))

## joining
address_sf <- address %>% 
  st_join(census_block_sf, join = st_within)

address_sd <- address %>% 
  st_join(census_block_sd, join = st_within)

## filtering to only the matched addresses within block
sf_mother_addresses <- address_sf %>% 
  st_drop_geometry() %>%
  filter(!is.na(NAME)) 

sd_mother_addresses <- address_sd %>% 
  st_drop_geometry() %>%
  filter(!is.na(NAME)) 



sf_mother_addresses <- sf_mother_addresses %>% 
  select(-ends_with("max"), -ends_with("min"),
         -status, -match_type, -value) 

sd_mother_addresses <- sd_mother_addresses %>% 
  select(-ends_with("max"), -ends_with("min"),
         -status, -match_type, -value) 

sf_mother_addresses %>% 
  write_csv("created_data/sf_addresses_censusblocks.csv")

sd_mother_addresses %>% 
  write_csv("created_data/sd_addresses_censusblocks.csv")
