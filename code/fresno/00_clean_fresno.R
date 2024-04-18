## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2023-10-03
##

library(tidyverse)
library(sf)
library(mapview)

fresno <- read_csv("added_data/fresno_sst.csv")
census_block_fresno <- st_read("data/shapefiles/fresno_census_data.shp")

fresno <- fresno %>% 
  mutate(datetime = mdy_hms(datetime),
         date = as_date(datetime),
         year = year(date),
         month = month(date),
         day = day(date),
         year_month = mdy(paste0(month, "-1-", year)))

fresno <- fresno %>% 
  drop_na(year_month)

fresno_geo <- fresno %>% 
  st_as_sf(coords = c("long", "lat"), crs = st_crs(census_block_fresno))


## spatial joining within the boundaries
fresno_geo_joined <- st_join(fresno_geo, census_block_fresno, join = st_within)


fresno_geo_joined <- fresno_geo_joined %>% 
  st_drop_geometry()

## waking hours
fresno_geo_joined <- fresno_geo_joined %>% 
  mutate(hour = hour(datetime),
         working_hours = if_else(hour %in% c(9:17), 1, 0)) 

# fresno %>% 
#   arrange(desc(datetime))

# creating panel of dates -------------------------------------------------

panel_dates <- birthsst::create_panel(start_date = "2015-07-01",
                                      end_date = "2018-08-01",
                                      by = "month")


## getting rid of certain days with gunfire heavy false-positive
fresno_geo_joined <- fresno_geo_joined %>% 
  birthsst::filter_false_positive_dates()

block_panel <- fresno_geo_joined %>% 
  st_drop_geometry() %>% 
  distinct(NAME,GEOID) %>% 
  cross_join(panel_dates)



fresno_geo_joined <- fresno_geo_joined %>% 
  group_by(year_month, GEOID, NAME) %>% 
  summarize(number_gunshot_working_hours = sum(working_hours),
            number_gunshots = n()) %>% 
  ungroup() %>% 
  arrange(desc(number_gunshots)) 

## change in expansion in Feb 2018. Getting rid any time after
## can add back in if needed
fresno_geo_joined <- fresno_geo_joined %>% 
  filter(year_month <= as_date("2018-01-01"))


fresno_geo_joined <- block_panel %>% 
  left_join(fresno_geo_joined,
            by = join_by(NAME == NAME,
                         GEOID == GEOID,
                         year_month == year_month))


## replacing NAs with 0s for the shots
fresno_geo_joined <- fresno_geo_joined %>% 
  mutate(across(matches("^number|^working"), ~replace_na(., 0)))


fresno_geo_joined <- fresno_geo_joined %>% 
  extract(NAME, into = c("census_block", "census_block_group", "census_tract"),
          "Block\\s(\\d{1,4}).{1,}Group\\s(\\d{1,2}),.{1,}Tract\\s(.{1,}),.{1,}", remove = F) %>%
  mutate(census_tract = parse_number(census_tract) %>% 
           as.character()) 

fresno_geo_joined <- fresno_geo_joined %>% 
  mutate(shotspotter_city = "Fresno") %>% 
  select(NAME, census_block, census_block_group,
         census_tract, GEOID, year_month, 
         number_gunshot_working_hours, number_gunshots,
         shotspotter_city) %>% 
  mutate(number_w_duplicate_areamatch = NA)


fresno_geo_joined %>% 
  write_csv("analysis_data/fresno_gunshots_blocks.csv")


