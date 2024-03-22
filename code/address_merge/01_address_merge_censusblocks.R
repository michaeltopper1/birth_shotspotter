## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2023-03-07
##

## this file will match the addresses to the census blocks

library(tidyverse)
library(sf)
library(mapview)


# loading in data ---------------------------------------------------------

address <- read_csv("data/all_addresses_with_coords.csv")

census_block_sf <- st_read("data/shapefiles/sf_census_data.shp")

census_block_sd <- st_read("data/shapefiles/sd_census_data.shp")

census_block_san_pablo <- st_read("data/shapefiles/sanpablo_census_data.shp")

census_block_fresno <- st_read("data/shapefiles/fresno_census_data.shp")

census_block_stockton <- st_read("data/shapefiles/stockton_census_data.shp")

census_block_oakland <- st_read("data/shapefiles/oakland_census_data.shp")

census_block_east_palo <- st_read("data/shapefiles/east_palo_alto_census_data.shp")

census_block_bakersfield <- st_read("data/shapefiles/bakersfield_census_data.shp")

# merging addresses to census blocks ------------------------------------

## first changing to the correct coordinate reference system
address <- address %>% 
  st_as_sf(coords = c("x", "y"), crs = st_crs(census_block_sf))

## joining
address_sf <- address %>% 
  st_join(census_block_sf, join = st_within)

address_pablo <- address %>% 
  st_join(census_block_san_pablo, join = st_within)

address_fresno <- address %>% 
  st_join(census_block_fresno, join = st_within)

address_stockton <- address %>% 
  st_join(census_block_stockton, join = st_within)

address_oakland <- address %>% 
  st_join(census_block_oakland, join = st_within)

address_east_palo <- address %>% 
  st_join(census_block_east_palo, join = st_within)

address_sd <- address %>% 
  st_join(census_block_sd, join = st_within)

address_bakersfield <- address %>% 
  st_join(census_block_bakersfield, join = st_within)

## filtering to only the matched addresses within block
sf_mother_addresses <- address_sf %>% 
  st_drop_geometry() %>%
  filter(!is.na(NAME)) 

pablo_mother_addresses <- address_pablo %>% 
  st_drop_geometry() %>%
  filter(!is.na(NAME)) 

fresno_mother_addresses <- address_fresno %>% 
  st_drop_geometry() %>%
  filter(!is.na(NAME)) 

stockton_mother_addresses <- address_stockton %>% 
  st_drop_geometry() %>%
  filter(!is.na(NAME)) 

oakland_mother_addresses <- address_oakland %>% 
  st_drop_geometry() %>%
  filter(!is.na(NAME)) 

east_palo_mother_addresses <- address_east_palo %>% 
  st_drop_geometry() %>% 
  filter(!is.na(NAME))

sd_mother_addresses <- address_sd %>% 
  st_drop_geometry() %>% 
  filter(!is.na(NAME))

bakersfield_mother_addresses <- address_bakersfield %>% 
  st_drop_geometry() %>% 
  filter(!is.na(NAME))

# pooling together the addresses ------------------------------------------

mother_addresses <- sf_mother_addresses %>% 
  bind_rows(stockton_mother_addresses,
            pablo_mother_addresses,
            fresno_mother_addresses,
            oakland_mother_addresses,
            east_palo_mother_addresses,
            sd_mother_addresses,
            bakersfield_mother_addresses)

mother_addresses <- mother_addresses %>% 
  select(-ends_with("max"), -ends_with("min"),
         -status, -match_type, -value) 



mother_addresses %>% 
  write_csv("analysis_data/addresses_censusblocks.csv")


