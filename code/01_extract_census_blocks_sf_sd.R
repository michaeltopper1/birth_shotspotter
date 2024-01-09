## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2023-03-08
##

library(tidyverse)
library(tidycensus)
library(sf)

## getting variables to add in. Not really too necessary,
## I can add in more later using this package, but I just needed something other than geometry
variables <- load_variables(2020, "pl", cache = TRUE) %>% 
  slice(1) %>% pull(name)

## grabbing the data (SF)
census_block_sf <- get_decennial(geography = "block",
                                 state = "CA",
                                 county = "San Francisco",
                                 variables = variables,
                                 year = 2020,
                                 geometry = T)

census_block_sf %>% 
  st_write("data/sf_census_data.shp")

## grabbing the data (SD)
census_block_sd <- get_decennial(geography = "block",
                                 state = "CA",
                                 county = "San Diego",
                                 variables = variables,
                                 year = 2020,
                                 geometry = T)


census_block_sd %>% 
  st_write("data/sd_census_data.shp")

## grabbing the data (FRESNO)
census_block_fresno <- get_decennial(geography = "block",
                                     state = "CA",
                                     county = "Fresno",
                                     variables = variables,
                                     year = 2020,
                                     geometry = T)

census_block_fresno %>% 
  st_write("data/fresno_census_data.shp")

## grabbing the data for stockton
census_block_stockton <- get_decennial(geography = "block",
                                       state = "CA",
                                       county = "San Joaquin County",
                                       variables = variables,
                                       year = 2020,
                                       geometry = T)
census_block_stockton %>% 
  st_write("data/stockton_census_data.shp")

## grabbing the data for Oakland (alemeda county)
census_block_oakland <- get_decennial(geography = "block",
                                       state = "CA",
                                       county = "Alameda County",
                                       variables = variables,
                                       year = 2020,
                                       geometry = T)
census_block_oakland %>% 
  st_write("data/oakland_census_data.shp")

## grabbing the data for San pablo
census_block_pablo <- get_decennial(geography = "block",
                                       state = "CA",
                                       county = "Contra Costa County",
                                       variables = variables,
                                       year = 2020,
                                       geometry = T)
census_block_pablo %>% 
  st_write("data/sanpablo_census_data.shp")
