## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2023-03-09
##

library(tidyverse)
library(sf)
library(mapview)

sd <- read_csv("data/sd_shotspotter_only_geocoded.csv") %>% janitor::clean_names()

census_block_sd <- st_read("data/sd_census_data.shp")


# adding sunrise/sunset ---------------------------------------------------

sd <- sd %>% 
  mutate(gunshot_date = as_date(date_time)) %>% 
  rename(gunshot_datetime = date_time) %>% 
  mutate(sunrise_time = photobiology::sunrise_time(date = gunshot_date,
                                                   tz = "America/Los_Angeles",
                                                   geocode = tibble::tibble(lon =-117.0601,
                                                                            lat =	32.69765,
                                                                            address = "San Diego")) %>% 
           hms::as_hms(),
         sunset_time = photobiology::sunset_time(date = gunshot_date,
                                                 tz = "America/Los_Angeles",
                                                 geocode = tibble::tibble(lon =-117.0601,
                                                                          lat =	32.69765,
                                                                          address = "San Diego")) %>% 
           hms::as_hms(),.before = 1) %>% 
  mutate(gunshot_daylight = if_else(between(hms::as_hms(gunshot_datetime), sunrise_time, sunset_time),
                                    1, 0), .before = 1) 


sd <- sd %>% 
  select(address, incident_num,
         starts_with("gunshot"), call_type,
         beat, priority,
         latitude, longitude, starts_with("accuracy"))

sd <- sd %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = st_crs(census_block_sd))

## looks liek all the san diego shots are coming from just around national city. This may be the only place
## they have shotspotter censors. If this is the case, then should only look at ever-treated 

sd_joined <- st_join(sd, census_block_sd, join = st_within)


sd_joined <- sd_joined %>% 
  add_count(incident_num) %>% 
  relocate(n) %>% 
  mutate(duplicate_area = ifelse(n > 1, 1, 0 ), .before = 1) %>% 
  distinct(incident_num, .keep_all = T) %>% 
  select(-n, -variable, - value, - beat)



# creating panel of dates -------------------------------------------------

panel_dates <- seq(as_date("2016-01-01"), as_date("2021-01-01") , by= "day") %>% 
  as_tibble() %>% 
  rename(date = value) %>% 
  mutate(month = month(date),
         year = year(date),
         day = day(date),
         week = week(date)) %>% 
  mutate(year_month = ymd(paste0(year, "-",month, "-1")))

## creates different groupings of the panel
block_panel <- sd_joined %>% 
  st_drop_geometry() %>% 
  distinct(NAME,GEOID) %>% 
  cross_join(panel_dates)



sd_joined_block_counts <- sd_joined %>%
  st_drop_geometry() %>% 
  group_by(gunshot_date, GEOID, NAME) %>% 
  summarize(number_w_duplicate_areamatch = sum(duplicate_area),
            number_gunshot_daylight = sum(gunshot_daylight),
            number_gunshots = n()) %>% 
  ungroup() %>% 
  arrange(desc(number_gunshots)) 


block_panel <- block_panel %>% 
  left_join(sd_joined_block_counts, by = join_by(GEOID == GEOID,
                                    NAME == NAME,
                                    date == gunshot_date))

## replacing NAs with 0s for the shots
block_panel <- block_panel %>% 
  mutate(across(starts_with("number"), ~replace_na(., 0)))

## NOTE: Block Panel is at the daily level. 
## Aggregating to the monthly level because of dimensionality issues.

sd_monthly_merged <- block_panel %>% 
  group_by(year_month, GEOID, NAME) %>% 
  summarize(across(starts_with("number"), ~sum(.))) %>% 
  extract(NAME, into = c("census_block", "census_block_group", "census_tract"),
          "Block\\s(\\d{1,4}).{1,}Group\\s(\\d{1,2}),.{1,}Tract\\s(.{1,}\\d{1,3}),.{1,}", remove = F) %>% 
  mutate(shotspotter_city = "San Diego")
  
sd_monthly_merged %>% 
  write_csv("created_data/sd_gunshots_blocks.csv")
