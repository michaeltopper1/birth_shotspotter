## Purpose of script: clean the old SF data
##
## Author: Michael Topper
##
## Date Last Edited: 2023-03-08
##

library(tidyverse)




sf_old <- read_csv("data/sanfrancisco.csv")


sf_old <- sf_old %>% 
  janitor::clean_names() %>% 
  mutate(date = dmy(date),
         city = "San Francisco",
         state = "CA") %>% 
  rename(numrounds = rnds,
         lat = latitude,
         long = longitude) %>% 
  select(-dispo) 

sf_old <- sf_old %>% 
  mutate(date_time_shot = ymd_hms(paste(date, time), tz = "America/Los_Angeles"), 
         .before =1 )

sf_old <- sf_old %>% 
  mutate(sunrise_time = photobiology::sunrise_time(date = date,
                                                   tz = "America/Los_Angeles",
                                                   geocode = tibble::tibble(lon =-122.3871,
                                                                            lat =	37.73105,
                                                                            address = "San Francisco")) %>% 
           hms::as_hms(),
         sunset_time = photobiology::sunset_time(date = date,
                                    tz = "America/Los_Angeles",
                                    geocode = tibble::tibble(lon =-122.3871,
                                                             lat =	37.73105,
                                                             address = "San Francisco")) %>% 
           hms::as_hms(),.before = 1) %>% 
  mutate(gunshot_daylight = if_else(between(hms::as_hms(date_time_shot), sunrise_time, sunset_time),
                                1, 0), .before = 1) 


sf_old <- sf_old %>% 
  rename(datetime_shot = date_time_shot,
         date_shot = date,
         time_shot = time,
         cad_number = id,
         latitude = lat,
         longitude = long) %>% 
  select(-beat, -city, - state) 

sf_old %>% 
  write_csv("created_data/sf_13_15_clean.csv")

  
