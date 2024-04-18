## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2023-03-08
##

library(tidyverse)
library(sf)
library(mapview)


# loading in data ---------------------------------------------------------

census_block_sf <- st_read("data/shapefiles/sf_census_data.shp")

## unsure if the cad_number for the old shotspotter stuff means that it wasn't actually
## a shot. Might just want to test out with the 2016 data first.
# sf_old <- read_csv("created_data/sf_13_15_clean.csv")
sf_new <- read_csv("created_data/sf_16_22_clean.csv")

# sf <- bind_rows(sf_old, sf_new)
sf <- sf_new

sf <- sf %>% 
  st_as_sf(coords = c("latitude", "longitude"), crs = st_crs(census_block_sf))

sf_joined <- st_join(sf, census_block_sf, join = st_within)


## only SST districts
sst_districts <- sf_joined %>% 
  count(police_district, sort = T) %>% 
  head(6) %>% pull(police_district)


sf_joined <- sf_joined %>% 
  filter(police_district %in% sst_districts)


# this shows waking hour shots  -------------------------------------------

sf_joined <- sf_joined %>% 
  mutate(year = year(date_shot),
         month = month(date_shot),
         day = day(date_shot),
         year_month = mdy(paste0(month, "-1-", year)))

## put in waking hours for 7am-11pm
sf_joined <- sf_joined %>% 
  mutate(hour = hour(datetime_shot),
         working_hours = if_else(hour %in% c(9:17), 1, 0))


# creating panel of dates -------------------------------------------------

panel_dates <- birthsst::create_panel(start_date = "2016-01-01",
                                      end_date = "2021-01-01",
                                      by = "month")

## getting rid of certain days with gunfire heavy false-positive
sf_joined <- sf_joined %>% 
  birthsst::filter_false_positive_dates()

block_panel <- sf_joined %>% 
  st_drop_geometry() %>% 
  distinct(NAME,GEOID) %>% 
  cross_join(panel_dates)

## randomly assigning the duplicates and creating an indicator for the duplicates
## the duplicates are the ones that were matched to multiple blocks
sf_joined <- sf_joined %>% 
  add_count(cad_number) %>% 
  relocate(n) %>% 
  mutate(duplicate_area = ifelse(n > 1, 1, 0 ), .before = 1) %>% 
  distinct(cad_number, .keep_all = T) %>% 
  select(-n)

sf_joined <- sf_joined %>% 
  st_drop_geometry() %>% 
  group_by(year_month, GEOID, NAME) %>% 
  summarize(number_w_duplicate_areamatch = sum(duplicate_area),
            number_gunshot_working_hours = sum(working_hours),
            number_gunshots = n()) %>% 
  ungroup() 


block_panel <- block_panel %>% 
  left_join(sf_joined,
            by = join_by(NAME == NAME,
                         GEOID == GEOID,
                         year_month == year_month))

## replacing NAs with 0s for the shots
block_panel <- block_panel %>% 
  mutate(across(starts_with("number"), ~replace_na(., 0)))


sf_monthly_merged <- block_panel %>% 
  extract(NAME, into = c("census_block", "census_block_group", "census_tract"),
          "Block\\s(\\d{1,4}).{1,}Group\\s(\\d{1,2}),.{1,}Tract\\s(.{1,}\\d{1,3}),.{1,}", remove = F) %>% 
  mutate(shotspotter_city = "San Francisco")


sf_monthly_merged %>% 
  write_csv("analysis_data/sf_gunshots_blocks.csv")
