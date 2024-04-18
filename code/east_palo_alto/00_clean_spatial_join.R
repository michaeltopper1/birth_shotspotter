library(tidyverse)

files <- list.files("added_data/east_palo/")

east_palo <- map_df(files, ~readxl::read_excel(paste0("added_data/east_palo/", .)))
census_block_east_palo_alto <- st_read("data/shapefiles/east_palo_alto_census_data.shp")


east_palo <- east_palo %>% 
  janitor::clean_names()

east_palo <- east_palo %>% 
  filter(type != "Not Gunfire")


## a couple of duplicates in there
east_palo <- east_palo %>% 
  distinct()

east_palo <- east_palo %>% 
  mutate(date_time = ymd_hms(date_time),
         date = as_date(date_time),
          year = year(date),
          month = month(date),
          day = day(date),
          year_month = mdy(paste0(month, "-1-", year)))



# converting to sf --------------------------------------------------------


east_palo <- east_palo %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = st_crs(census_block_east_palo_alto))



# spatial joining ---------------------------------------------------------


east_palo_joined <- st_join(east_palo, census_block_east_palo_alto, join = st_within)


## dropping geometry for joining purposes
east_palo_joined <- east_palo_joined %>% 
  st_drop_geometry()

east_palo_joined <- east_palo_joined %>% 
  mutate(time = hms::as_hms(date_time),
         hour = hour(time),
         working_hours = if_else(hour %in% c(9:17), 1, 0), .before = 1) 
east_palo_joined %>% 
  arrange(desc(date))



# creating panel of dates -------------------------------------------------


## need to get the earliest date of implementation and final date of SST
## going to use august instead of july to get a full month
panel_dates <- birthsst::create_panel(start_date = "2008-07-01",
                                      end_date = "2019-08-01",
                                      by = "month")


## getting rid of certain days with gunfire heavy false-positive
east_palo_joined <- east_palo_joined %>% 
  birthsst::filter_false_positive_dates()


block_panel <- east_palo_joined %>% 
  distinct(NAME,GEOID) %>% 
  cross_join(panel_dates)



# aggregating -------------------------------------------------------------

east_palo_geo <- east_palo_joined %>% 
  group_by(year_month, GEOID, NAME) %>% 
  summarize(number_gunshot_working_hours = sum(working_hours),
            number_gunshots = n()) %>% 
  ungroup() %>% 
  arrange(desc(number_gunshots)) 

## getting rid of any dates that are after enhanced coverage.
east_palo_geo <- east_palo_geo %>% 
  filter(year_month < as_date("2019-09-01"))

east_palo_geo <- east_palo_geo%>% 
  mutate(across(matches("^number|^working"), ~replace_na(., 0)))

east_palo_geo <- east_palo_geo %>% 
  extract(NAME, into = c("census_block", "census_block_group", "census_tract"),
          "Block\\s(\\d{1,4}).{1,}Group\\s(\\d{1,2}),.{1,}Tract\\s(.{1,}),.{1,}", remove = F) %>%
  mutate(census_tract = parse_number(census_tract) %>% 
           as.character()) 

east_palo_geo <- east_palo_geo %>% 
  mutate(shotspotter_city = "East Palo Alto") %>% 
  select(NAME, census_block, census_block_group,
         census_tract, GEOID, year_month, 
         number_gunshot_working_hours, number_gunshots,
         shotspotter_city) %>% 
  mutate(number_w_duplicate_areamatch = NA)

east_palo_geo %>% 
  write_csv("analysis_data/east_palo_gunshots_blocks.csv")



