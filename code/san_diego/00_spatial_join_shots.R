library(tidyverse)
library(sf)
library(mapview)


sd <- read_csv("data/sd_shotspotter_only_geocoded.csv")
census_blocks_sd <- st_read("data/shapefiles/sd_census_data.shp")

sd <- sd %>% 
  distinct()


sd <- sd %>% 
  mutate(date_time = ymd_hms(date_time),
         date = as_date(date_time),
         year = year(date),
         month = month(date),
         day = day(date),
         year_month = mdy(paste0(month, "-1-", year)))


# converting to sf --------------------------------------------------------

sd <- sd %>% 
  st_as_sf(coords = c("Longitude", "Latitude"), crs = st_crs(census_blocks_sd))



# spatial joining ---------------------------------------------------------


sd <- st_join(sd, census_blocks_sd, join = st_within)


## dropping geometry for joining purposes
sd <- sd %>% 
  st_drop_geometry()

sd <- sd %>% 
  mutate(time = hms::as_hms(date_time),
         hour = hour(time),
         working_hours = if_else(hour %in% c(9:17), 1, 0), .before = 1) 
sd %>% 
  arrange((date_time))



# creating panel of dates -------------------------------------------------


## need to get the earliest date of implementation and final date of SST
## going to use august instead of july to get a full month
panel_dates <- birthsst::create_panel(start_date = "2017-01-01",
                                      end_date = "2020-12-01",
                                      by = "month")


## getting rid of certain days with gunfire heavy false-positive
sd_joined <- sd %>% 
  birthsst::filter_false_positive_dates()


block_panel <- sd_joined %>% 
  distinct(NAME,GEOID) %>% 
  cross_join(panel_dates)


# aggregating -------------------------------------------------------------

sd_joined <- sd_joined %>% 
  group_by(year_month, GEOID, NAME) %>% 
  summarize(number_gunshot_working_hours = sum(working_hours),
            number_gunshots = n()) %>% 
  ungroup() %>% 
  arrange(desc(number_gunshots)) 


sd_joined <- sd_joined %>% 
  mutate(across(matches("^number|^working"), ~replace_na(., 0)))

sd_joined <- sd_joined %>% 
  extract(NAME, into = c("census_block", "census_block_group", "census_tract"),
          "Block\\s(\\d{1,4}).{1,}Group\\s(\\d{1,2}),.{1,}Tract\\s(.{1,}),.{1,}", remove = F) %>%
  mutate(census_tract = parse_number(census_tract) %>% 
           as.character()) 

sd_joined <- sd_joined %>% 
  mutate(shotspotter_city = "San Diego") %>% 
  select(NAME, census_block, census_block_group,
         census_tract, GEOID, year_month, 
         number_gunshot_working_hours, number_gunshots,
         shotspotter_city) %>% 
  mutate(number_w_duplicate_areamatch = NA)

sd_joined %>% 
  write_csv("analysis_data/sd_gunshots_blocks.csv")



