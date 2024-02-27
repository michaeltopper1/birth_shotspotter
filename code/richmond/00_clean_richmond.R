library(tidyverse)
library(sf)


richmond <- read_csv("added_data/richmond_ca.csv")
richmond_addresses <- read_csv("address_data/richmond_addresses_geocodio.csv")
## same census lines as san pablo - they are right next to each other on the map
census_block_richmond <- st_read("data/shapefiles/sanpablo_census_data.shp")


richmond <- richmond %>% 
  mutate(date = mdy(date),
          year = year(date),
          month = month(date),
          day = day(date),
          year_month = mdy(paste0(month, "-1-", year)))

richmond <- richmond %>% 
  mutate(city = "Richmond",
         state = "CA")

richmond <- richmond %>% 
  select(type, id, date, time, address, year, month, day, city , state, year_month)

richmond <- richmond %>% 
  distinct(id, .keep_all = T)

## there are some gunshot/firecracker types, however, since there is still the 
## possibility of a gunshot, I am keeping these in here
## these are all reveiewed by human reviewers, and if they can't tell, neither can 
## a mother 


# cleaning addresses ------------------------------------------------------

richmond_addresses <- richmond_addresses %>% 
  janitor::clean_names() %>% 
  select(address, latitude, longitude)

richmond_addresses <- richmond_addresses %>% 
  distinct()


# re-merging addresses with sst data --------------------------------------

richmond <- richmond %>% 
  left_join(richmond_addresses, join_by(address == address))


richmond_geo <- richmond %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = st_crs(census_block_richmond))

richmond_geo <- st_join(richmond_geo, census_block_richmond, join = st_within)



## dropping geometry for joining purposes
richmond_geo <- richmond_geo %>% 
  st_drop_geometry()

richmond_geo <- richmond_geo %>% 
  mutate(hour = hour(time),
         waking_hours = if_else(hour %in% c(6:23), 1, 0), .before = 1) 


# creating panel of dates -------------------------------------------------


## need to get the earliest date of implementation and final date of SST
## going to use august instead of july to get a full month
panel_dates <- birthsst::create_panel(start_date = "2009-04-01",
                                      end_date = "2018-10-01",
                                      by = "month")


## getting rid of certain days with gunfire heavy false-positive
richmond_geo <- richmond_geo %>% 
  birthsst::filter_false_positive_dates()


block_panel <- richmond_geo %>% 
  distinct(NAME,GEOID) %>% 
  cross_join(panel_dates)

# aggregating -------------------------------------------------------------

richmond_geo <- richmond_geo %>% 
  group_by(year_month, GEOID, NAME) %>% 
  summarize(number_gunshot_waking_hours = sum(waking_hours),
            number_gunshots = n()) %>% 
  ungroup() %>% 
  arrange(desc(number_gunshots))

richmond_geo <- richmond_geo %>% 
  mutate(across(matches("^number|^waking"), ~replace_na(., 0)))

richmond_geo <- richmond_geo %>% 
  extract(NAME, into = c("census_block", "census_block_group", "census_tract"),
          "Block\\s(\\d{1,4}).{1,}Group\\s(\\d{1,2}),.{1,}Tract\\s(.{1,}),.{1,}", remove = F) %>%
  mutate(census_tract = parse_number(census_tract) %>% 
           as.character()) 


richmond_geo <- richmond_geo %>% 
  mutate(shotspotter_city = "Richmond") %>% 
  select(NAME, census_block, census_block_group,
         census_tract, GEOID, year_month, 
         number_gunshot_waking_hours, number_gunshots,
         shotspotter_city) %>% 
  mutate(number_w_duplicate_areamatch = NA)


richmond_geo %>% 
  write_csv("analysis_data/richmond_gunshots_blocks.csv")
