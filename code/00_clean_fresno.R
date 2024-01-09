## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2023-10-03
##

library(tidyverse)
library(sf)
library(mapview)


fresno <- readxl::read_excel("added_data/fresno.xlsx") %>% janitor::clean_names()
fresno_address_geocode <- read_csv("address_data/fresno_addresses_geocodio.csv")

## for spatial join
census_block_fresno <- st_read("data/fresno_census_data.shp")


fresno <- fresno %>% 
  mutate(date = as_date(rcv_time),
          year = year(date),
          month = month(date),
          day = day(date),
          year_month = mdy(paste0(month, "-1-", year)))
  
# fresno <- fresno %>% 
#   filter(!(month ==7 & day == 4)) %>% 
#   filter(!(month == 1 & day == 1)) %>% 
#   filter(!(month == 12 & day == 31)) 

## 10k. about 9 dollars to geocode.
# fresno_addresses <- fresno %>% 
#   distinct(addr_st, addr_xst) %>% 
#   mutate(city = "Fresno",
#          state = "CA")
# 
# fresno_addresses <- fresno_addresses %>% 
#   mutate(address = glue::glue("{addr_st} and {addr_xst}"))
# 
# 
# fresno_addresses %>% 
#   write_csv("address_data/fresno_addresses.csv")


fresno <- fresno %>% 
  left_join(fresno_address_geocode, by = c("addr_st", "addr_xst")) %>% 
  janitor::clean_names()

fresno_geo <- fresno %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = st_crs(census_block_fresno))

## spatial joining within the boundaries
fresno_geo_joined <- st_join(fresno_geo, census_block_fresno, join = st_within)

## dropping the geometry to merge
fresno_geo_joined <- fresno_geo_joined %>% 
  st_drop_geometry()

fresno_geo_joined <- fresno_geo_joined %>% 
  distinct(rcv_time,clear_time,s1_request_time,  .keep_all = T)

## waking hours
fresno_geo_joined <- fresno_geo_joined %>% 
  mutate(hour = hour(rcv_time),
         waking_hours = if_else(hour %in% c(6:23), 1, 0)) 


# creating panel of dates -------------------------------------------------

panel_dates <- seq(as_date("2016-01-01"), as_date("2021-01-01") , by= "month") %>% 
  as_tibble() %>% 
  rename(date = value) %>% 
  mutate(month = month(date),
         year = year(date),
         day = day(date),
         week = week(date)) %>% 
  mutate(year_month = ymd(paste0(year, "-",month, "-1")))

block_panel <- fresno_geo_joined %>% 
  st_drop_geometry() %>% 
  distinct(NAME,GEOID) %>% 
  cross_join(panel_dates)


# aggregating -------------------------------------------------------------

fresno_geo_joined <- fresno_geo_joined %>% 
  group_by(year_month, GEOID, NAME, waking_hours) %>% 
  summarize(number_sst = n()) %>% 
  ungroup()



fresno_geo_joined <- block_panel %>% 
  left_join(fresno_geo_joined,
            by = join_by(NAME == NAME,
                         GEOID == GEOID,
                         year_month == year_month))


## replacing NAs with 0s for the shots
fresno_geo_joined <- fresno_geo_joined %>% 
  mutate(across(matches("^number|^waking"), ~replace_na(., 0)))


fresno_geo_joined <- fresno_geo_joined %>% 
  extract(NAME, into = c("census_block", "census_block_group", "census_tract"),
          "Block\\s(\\d{1,4}).{1,}Group\\s(\\d{1,2}),.{1,}Tract\\s(.{1,}),.{1,}", remove = F) %>%
  mutate(census_tract = parse_number(census_tract) %>% 
           as.character()) 

fresno_geo_joined <- fresno_geo_joined %>% 
  rename(number_gunshot_waking_hours = waking_hours,
         number_gunshots = number_sst) %>% 
  mutate(shotspotter_city = "Fresno") %>% 
  select(NAME, census_block, census_block_group,
         census_tract, GEOID, year_month, 
         number_gunshot_waking_hours, number_gunshots,
         shotspotter_city) %>% 
  mutate(number_w_duplicate_areamatch = NA)

fresno_geo_joined %>% 
  write_csv("analysis_data/fresno_gunshots_blocks.csv")


# should we get rid of certain days ---------------------------------------
## yes we should




## in fresno, they went through a change in coverage in feb 2018 or around this time
## documentation is in the zotero.



## nearly 900 every month
## line graph confirming data is there for all 2016-2020
# fresno %>% 
#   arrange(desc(s1_request_time)) %>% 
#   mutate(date = as_date(s1_request_time),
#           year = year(date),
#           month = month(date),
#           day = day(date),
#           year_month = mdy(paste0(month, "-1-", year))) %>% 
#   count(year_month) %>% 
#   ggplot(aes(year_month, n)) +
#   geom_line()
#   
