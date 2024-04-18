## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2024-01-10
##

library(tidyverse)
library(sf)

stockton <- read_csv("added_data/stockton_ca.csv")
stockton_addresses <- read_csv("address_data/stockton_addresses_geocodio.csv")
census_block_stockton <- st_read("data/shapefiles/stockton_census_data.shp")

stockton <- stockton %>% 
  mutate(datetime = mdy_hm(datetimereceived), .before =1) %>% 
  mutate(date = as_date(datetime),
          year = year(date),
          month = month(date),
          day = day(date),
          year_month = mdy(paste0(month, "-1-", year)))



# cleaning the address data for mergining ---------------------------------

## selecting only the columns necessary
stockton_addresses <- stockton_addresses %>% 
  janitor::clean_names() %>% 
  select(address, latitude, longitude)

## removing any duplicates in address geocodes
stockton_addresses <- stockton_addresses %>% 
  distinct()



# merging the address data with SST data --------------------------------

stockton <- stockton %>% 
  left_join(stockton_addresses, join_by(eventaddress == address))


# getting to sf -----------------------------------------------------------

stockton_geo <- stockton %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = st_crs(census_block_stockton))


stockton_geo <- st_join(stockton_geo, census_block_stockton, join = st_within)


## dropping geometry for joining purposes
stockton_geo <- stockton_geo %>% 
  st_drop_geometry()

stockton_geo <- stockton_geo %>% 
  mutate(time = hms::as_hms(datetime),
         hour = hour(time),
         working_hours = if_else(hour %in% c(9:17), 1, 0), .before = 1) 




# creating panel of dates -------------------------------------------------


## need to get the earliest date of implementation and final date of SST
## going to use august instead of july to get a full month
panel_dates <- birthsst::create_panel(start_date = "2013-08-01",
                                      end_date = "2018-08-01",
                                      by = "month")


## getting rid of certain days with gunfire heavy false-positive
stockton_geo <- stockton_geo %>% 
  birthsst::filter_false_positive_dates()


block_panel <- stockton_geo %>% 
  distinct(NAME,GEOID) %>% 
  cross_join(panel_dates)


# aggregating -------------------------------------------------------------

stockton_geo <- stockton_geo %>% 
  group_by(year_month, GEOID, NAME) %>% 
  summarize(number_gunshot_working_hours = sum(working_hours),
            number_gunshots = n()) %>% 
  ungroup() %>% 
  arrange(desc(number_gunshots)) 


stockton_geo <- stockton_geo %>% 
  mutate(across(matches("^number|^working"), ~replace_na(., 0)))

stockton_geo <- stockton_geo %>% 
  extract(NAME, into = c("census_block", "census_block_group", "census_tract"),
          "Block\\s(\\d{1,4}).{1,}Group\\s(\\d{1,2}),.{1,}Tract\\s(.{1,}),.{1,}", remove = F) %>%
  mutate(census_tract = parse_number(census_tract) %>% 
           as.character()) 

stockton_geo <- stockton_geo %>% 
  mutate(shotspotter_city = "Stockton") %>% 
  select(NAME, census_block, census_block_group,
         census_tract, GEOID, year_month, 
         number_gunshot_working_hours, number_gunshots,
         shotspotter_city) %>% 
  mutate(number_w_duplicate_areamatch = NA)

stockton_geo %>% 
  write_csv("analysis_data/stockton_gunshots_blocks.csv")

