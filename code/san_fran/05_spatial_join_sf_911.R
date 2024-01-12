## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2023-09-25
##

library(tidyverse)
library(sf)
library(mapview)


# reading in data ---------------------------------------------------------

## all 911 dispatches or "on-views"
sf_911 <- read_csv("data/sf_911.csv")

## for spatial join
census_block_sf <- st_read("data/sf_census_data.shp")

## violent crime descriptions that i personally indicated
violent_crimes <- read_csv('created_data/911_descriptions.csv')

## shotspotter alerts in sanfran - only used to get the districts with sst
sst <- read_csv("data/sanfran_1622.csv")




## getting the districts with SST
sst_districts <- sst %>% 
  count(police_district, sort = T) %>% 
  head(6) %>% pull(police_district)

## getting only the distinct geo locations of the intersections
sf_crime_coords <- sf_911 %>% 
  distinct(intersection_point)

## extracting the points into new column to change to an SF object
sf_crime_coords <- sf_crime_coords %>% 
  extract(intersection_point, into = c("latitude", "longitude"),
          "POINT\\s\\((.{1,})\\s(.{1,})\\)", remove = F) %>% 
  mutate(across(everything(), ~str_trim(.))) %>% 
  drop_na() 

## changing to a SF object. Use mapview to check that this is correct (it is)
sf_crime_geo <- sf_crime_coords %>% 
  st_as_sf(coords = c("latitude", "longitude"), crs = st_crs(census_block_sf)) 

## spatial joining within the boundaries
sf_crime_geo_joined <- st_join(sf_crime_geo, census_block_sf, join = st_within)

## dropping the geometry to merge
sf_crime_geo_joined <- sf_crime_geo_joined %>% 
  st_drop_geometry()

## some intersection points map to more than one boundary. 
sf_crime_geo_joined <- sf_crime_geo_joined %>% 
  distinct(intersection_point, .keep_all = T)

## joining
sf_911 <- sf_911 %>% 
  left_join(sf_crime_geo_joined)


## there is only about 1000 missing intersections each month
## in which there are missing intersection points.
sf_911 <- sf_911 %>% 
  mutate(missing_intersection = if_else(is.na(intersection_name), 1, 0)) %>% 
  mutate(date = mdy_hms(received_datetime),
          year = year(date),
          month = month(date),
          day = day(date),
          year_month = mdy(paste0(month, "-1-", year))) 

## dropping any NA points that won't merge
sf_911 <- sf_911 %>% 
  drop_na(intersection_point)

violent_crimes <- violent_crimes %>% 
 mutate(across(c(violent_crime, homicide), ~if_else(is.na(.), 0, .))) 

violent_crime_descriptions <- violent_crimes %>% 
  filter(violent_crime == 1) %>% pull(call_type_final_desc)


# delineating between violent crimes --------------------------------------

sf_911 <- sf_911 %>% 
  mutate(violent_crime = if_else(call_type_final_desc %in% violent_crime_descriptions,
                                 1, 0),
         homicide = if_else(call_type_final_desc == "HOMICIDE",1,0))


# delineating between waking hours/nonwaking ------------------------------

sf_911 <- sf_911 %>% 
  mutate(hour = hour(date),
         waking_hours = if_else(hour %in% c(6:23), 1, 0))

sf_911 <- sf_911 %>% 
  filter(year > 2015 & year < 2021) 


# filtering to only police districts that have sst ------------------------

sf_911 <- sf_911 %>% 
  filter(police_district %in% sst_districts)



# pivoting to get the data into correct format ----------------------------

sf_911_monthly <- sf_911 %>% 
  group_by(year_month, GEOID, NAME, waking_hours, violent_crime) %>% 
  summarize(number_crimes = n())


sf_911_monthly <- sf_911_monthly %>% 
  mutate(waking_hours = if_else(waking_hours == 1, "waking_hour_crime",
                                "non_waking_hour_crime")) %>% 
  mutate(violent_crime = if_else(violent_crime == 1, "violent", "nonviolent")) %>% 
  pivot_wider(names_from = waking_hours, values_from = number_crimes) %>% 
  pivot_wider(names_from = violent_crime, values_from =c(waking_hour_crime,
                                                        non_waking_hour_crime))



# replacing any NAs with 0s -----------------------------------------------

sf_911_monthly <- sf_911_monthly %>% 
  mutate(across(matches("^waking|^non"), ~replace_na(.,0))) 


sf_911_monthly %>% 
  write_csv("analysis_data/xxcrime_geocoded.csv")

