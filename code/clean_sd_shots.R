## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2023-03-09
##

library(tidyverse)

files <- list.files("data/", pattern = "^sd_calls")

sd <- map_df(files, ~read_csv(paste0("data/", .),
                              col_types = cols(
                                incident_num = col_character(),
                                date_time = col_datetime(format = ""),
                                day_of_week = col_double(),
                                address_number_primary = col_character(),
                                address_dir_primary = col_character(),
                                address_road_primary = col_character(),
                                address_sfx_primary = col_character(),
                                address_dir_intersecting = col_character(),
                                address_road_intersecting = col_character(),
                                address_sfx_intersecting = col_character(),
                                call_type = col_character(),
                                disposition = col_character(),
                                beat = col_character(),
                                priority = col_character()
                              )))

sd_15 <- read_csv("/Users/michaeltopper/Desktop/pd_calls_for_service_2015_datasd_v1.csv")
sd_15 %>% 
  filter(call_type == "11-6SPT")
sd %>% 
  mutate(year = year(date_time)) %>% 
  filter(year == 2016) %>% 
  filter(call_type == "11-6SPT") %>% 
  View()

sd_calltypesp <- read_csv('/Users/michaeltopper/Desktop/pd_cfs_calltypes_datasd.csv')

sd_calltypesp %>% 
  mutate(across(where(is.character), ~str_to_lower(.))) %>% 
  filter(str_detect(description, "firearm"))
sd_shotspotter <- sd %>% 
  filter(call_type == "11-6SPT") %>% 
  mutate(across(starts_with("address"), ~replace_na(., ""))) %>% 
  mutate(address = paste(address_number_primary, address_dir_primary, address_road_primary,
                          address_sfx_primary) %>% 
           str_replace("\\s\\s", " "), .before = 1) %>% 
  mutate(city = "San Diego",
         state = "California",
         country = "United States") %>% 
  select(-starts_with("address_"), -day_of_week)




sd_shotspotter %>% 
  write_csv("data/sd_shotspotter_only.csv")
