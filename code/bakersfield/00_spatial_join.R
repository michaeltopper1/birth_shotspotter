## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2023-10-03
##

library(tidyverse)
library(sf)
library(mapview)


bakersfield <- read_csv("address_data/bakersfield_addresses_geocodio.csv") %>% 
  janitor::clean_names() %>% 
  mutate(date = mdy(date))
census_block_bakersfield <- st_read("data/shapefiles/bakersfield_census_data.shp")

bakersfield <- bakersfield %>%
  filter(latitude != 0) 

bakersfield <- bakersfield %>% 
  mutate(datetime = date,
          date = as_date(date),
         year = year(date),
         month = month(date),
         day = day(date),
         year_month = mdy(paste0(month, "-1-", year)))

bakersfield_geo <- bakersfield %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = st_crs(census_block_bakersfield))

## spatial joining within the boundaries
bakersfield_geo <- st_join(bakersfield_geo, census_block_bakersfield, join = st_within)


bakersfield_geo <- bakersfield_geo %>% 
  st_drop_geometry()


## waking hours
bakersfield_geo <- bakersfield_geo %>% 
  mutate(hour = hour(time),
         waking_hours = if_else(hour %in% c(6:23), 1, 0)) 



# creating panel of dates -------------------------------------------------

panel_dates <- birthsst::create_panel(start_date = "2018-03-01",
                                      end_date = "2020-12-01",
                                      by = "month")


## getting rid of certain days with gunfire heavy false-positive
bakersfield_geo <- bakersfield_geo %>% 
  birthsst::filter_false_positive_dates()

block_panel <- bakersfield_geo %>% 
  st_drop_geometry() %>% 
  distinct(NAME,GEOID) %>% 
  cross_join(panel_dates)



bakersfield_geo <- bakersfield_geo %>% 
  group_by(year_month, GEOID, NAME) %>% 
  summarize(number_gunshot_waking_hours = sum(waking_hours),
            number_gunshots = n()) %>% 
  ungroup() %>% 
  arrange(desc(number_gunshots)) 



bakersfield_geo_joined <- block_panel %>% 
  left_join(bakersfield_geo,
            by = join_by(NAME == NAME,
                         GEOID == GEOID,
                         year_month == year_month))


## replacing NAs with 0s for the shots
bakersfield_geo_joined <- bakersfield_geo_joined %>% 
  mutate(across(matches("^number|^waking"), ~replace_na(., 0)))


bakersfield_geo_joined <- bakersfield_geo_joined %>% 
  extract(NAME, into = c("census_block", "census_block_group", "census_tract"),
          "Block\\s(\\d{1,4}).{1,}Group\\s(\\d{1,2}),.{1,}Tract\\s(.{1,}),.{1,}", remove = F) %>%
  mutate(census_tract = parse_number(census_tract) %>% 
           as.character()) 

bakersfield_geo_joined <- bakersfield_geo_joined %>% 
  mutate(shotspotter_city = "Bakersfield") %>% 
  select(NAME, census_block, census_block_group,
         census_tract, GEOID, year_month, 
         number_gunshot_waking_hours, number_gunshots,
         shotspotter_city) %>% 
  mutate(number_w_duplicate_areamatch = NA)

 
bakersfield_geo_joined %>% 
  write_csv("analysis_data/bakersfield_gunshots_blocks.csv")

