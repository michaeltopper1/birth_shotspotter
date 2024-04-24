library(tidyverse)
library(sf)
library(mapview)

oakland <- read_csv("added_data/oakland.csv") %>% 
  janitor::clean_names()

census_block_oakland <- st_read("data/shapefiles/oakland_census_data.shp")

oakland <- oakland %>% 
  st_as_sf(coords = c("xcoord", "ycoord"), crs = st_crs(census_block_oakland)) 

# spatial joining ---------------------------------------------------------


oakland_joined <- st_join(oakland, census_block_oakland, join = st_within)


## dropping geometry for joining purposes
oakland_joined <- oakland_joined %>% 
  st_drop_geometry()

oakland_joined <- oakland_joined %>% 
  mutate(date_time = mdy_hm(date_tim),
         time = hms::as_hms(date_time),
         hour = hour(time),
         working_hours = if_else(hour %in% c(9:17), 1, 0), 
         .before = 1)

oakland_joined <- oakland_joined %>% 
  mutate(date = as_date(date_time),
         year = year(date),
         month = month(date),
         day = day(date),
         year_month = mdy(paste0(month, "-1-", year)))

## starts in 2008-01-02 ends in 2013-10-06
## 2010 is good until December: lots of missing dates

## 2011 not good until August.

oakland_joined_1 <- oakland_joined %>% 
  filter(as_date(date_time) < as_date("2010-12-01"))

oakland_joined_2 <- oakland_joined %>% 
  filter(as_date(date_time) >= as_date("2011-08-01"))
  

# creating panel of dates -------------------------------------------------


## need to get the earliest date of implementation and final date of SST
## going to use august instead of july to get a full month
panel_dates <- birthsst::create_panel(start_date = "2008-01-01",
                                      end_date = "2010-11-01",
                                      by = "month")

panel_dates_2 <- birthsst::create_panel(start_date = "2011-08-01",
                                        end_date = "2013-09-01",
                                        by = "month")

## getting rid of certain days with gunfire heavy false-positive
oakland_joined_1 <- oakland_joined_1 %>% 
  birthsst::filter_false_positive_dates()

oakland_joined_2 <- oakland_joined_2 %>% 
  birthsst::filter_false_positive_dates()

block_panel_1 <- oakland_joined_1 %>% 
  distinct(NAME,GEOID) %>% 
  cross_join(panel_dates) 

block_panel_2 <- oakland_joined_2 %>%
  distinct(NAME,GEOID) %>% 
  cross_join(panel_dates)

# aggregating -------------------------------------------------------------

oakland_joined_1 <- oakland_joined_1 %>% 
  group_by(year_month, GEOID, NAME) %>% 
  summarize(number_gunshot_working_hours = sum(working_hours),
            number_gunshots = n()) %>% 
  ungroup() %>% 
  arrange(desc(number_gunshots)) 

oakland_joined_2 <- oakland_joined_2 %>% 
  group_by(year_month, GEOID, NAME) %>% 
  summarize(number_gunshot_working_hours = sum(working_hours),
            number_gunshots = n()) %>% 
  ungroup() %>% 
  arrange(desc(number_gunshots)) 

oakland_joined_1 <- oakland_joined_1 %>% 
  mutate(across(matches("^number|^working"), ~replace_na(., 0)))

oakland_joined_2 <- oakland_joined_2 %>% 
  mutate(across(matches("^number|^working"), ~replace_na(., 0)))

oakland_joined_1 <- oakland_joined_1 %>% 
  extract(NAME, into = c("census_block", "census_block_group", "census_tract"),
          "Block\\s(\\d{1,4}).{1,}Group\\s(\\d{1,2}),.{1,}Tract\\s(.{1,}),.{1,}", remove = F) %>%
  mutate(census_tract = parse_number(census_tract) %>% 
           as.character()) 

oakland_joined_2 <- oakland_joined_2 %>% 
  extract(NAME, into = c("census_block", "census_block_group", "census_tract"),
          "Block\\s(\\d{1,4}).{1,}Group\\s(\\d{1,2}),.{1,}Tract\\s(.{1,}),.{1,}", remove = F) %>%
  mutate(census_tract = parse_number(census_tract) %>% 
           as.character()) 

oakland_joined_1 <- oakland_joined_1 %>% 
  mutate(shotspotter_city = "Oakland") %>% 
  select(NAME, census_block, census_block_group,
         census_tract, GEOID, year_month, 
         number_gunshot_working_hours, number_gunshots,
         shotspotter_city) %>% 
  mutate(number_w_duplicate_areamatch = NA)

oakland_joined_2 <- oakland_joined_2 %>% 
  mutate(shotspotter_city = "Oakland") %>% 
  select(NAME, census_block, census_block_group,
         census_tract, GEOID, year_month, 
         number_gunshot_working_hours, number_gunshots,
         shotspotter_city) %>% 
  mutate(number_w_duplicate_areamatch = NA)


oakland_joined_1 %>% 
  write_csv("analysis_data/oakland_gunshots_blocks_1.csv")


oakland_joined_2 %>% 
  write_csv("analysis_data/oakland_gunshots_blocks_2.csv")
