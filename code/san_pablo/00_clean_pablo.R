## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2023-10-03
##

library(tidyverse)
library(sf)
library(mapview)

# importing data ----------------------------------------------------------

pablo <- readxl::read_excel("added_data/pablo.xlsx") %>% janitor::clean_names()

census_block_pablo <- st_read("data/shapefiles/sanpablo_census_data.shp")



# cleaning pablo data to get rid of dupes ---------------------------------

pablo <- pablo %>% 
  distinct(respond_id, .keep_all = T)

## changing from stupid excel time to regular time
pablo <- pablo %>% 
  mutate(time = hms::as_hms(time))

## getting rid of NAs for lat/long. only happens for 1 row
pablo <- pablo %>% 
  drop_na(latitude, longitude)

pablo <- pablo %>% 
  mutate(date = as_date(date),
          year = year(date),
          month = month(date),
          day = day(date),
          year_month = mdy(paste0(month, "-1-", year)))


# translating to spatial --------------------------------------------------

pablo_geo <- pablo %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = st_crs(census_block_pablo))

## checking that it is where it should be
# pablo_geo %>% mapview()


# spatial joining ---------------------------------------------------------

pablo_geo_joined <- st_join(pablo_geo, census_block_pablo, join = st_within)


## dropping geometry for joining purposes
pablo_geo_joined <- pablo_geo_joined %>% 
  st_drop_geometry()

pablo_geo_joined <- pablo_geo_joined %>% 
  mutate(hour = hour(time),
         waking_hours = if_else(hour %in% c(6:23), 1, 0), .before = 1) 


# creating panel of dates -------------------------------------------------

panel_dates <- birthsst::create_panel(start_date = "2013-07-01",
                                      end_date = "2021-01-01",
                                      by = "month")

## getting rid of certain days with gunfire heavy false-positive
pablo_geo_joined <- pablo_geo_joined %>% 
  birthsst::filter_false_positive_dates()

block_panel <- pablo_geo_joined %>% 
  distinct(NAME,GEOID) %>% 
  cross_join(panel_dates)


# aggregating -------------------------------------------------------------

pablo_geo_joined <- pablo_geo_joined %>% 
  group_by(year_month, GEOID, NAME) %>% 
  summarize(number_gunshot_waking_hours = sum(waking_hours),
            number_gunshots = n()) %>% 
  ungroup() %>% 
  arrange(desc(number_gunshots)) 


pablo_geo_joined <- pablo_geo_joined %>% 
  mutate(across(matches("^number|^waking"), ~replace_na(., 0)))

pablo_geo_joined <- pablo_geo_joined %>% 
  extract(NAME, into = c("census_block", "census_block_group", "census_tract"),
          "Block\\s(\\d{1,4}).{1,}Group\\s(\\d{1,2}),.{1,}Tract\\s(.{1,}),.{1,}", remove = F) %>%
  mutate(census_tract = parse_number(census_tract) %>% 
           as.character()) 

pablo_geo_joined <- pablo_geo_joined %>% 
  mutate(shotspotter_city = "San Pablo") %>% 
  select(NAME, census_block, census_block_group,
         census_tract, GEOID, year_month, 
         number_gunshot_waking_hours, number_gunshots,
         shotspotter_city) %>% 
  mutate(number_w_duplicate_areamatch = NA)

pablo_geo_joined %>% 
  write_csv("analysis_data/pablo_gunshots_blocks.csv")


