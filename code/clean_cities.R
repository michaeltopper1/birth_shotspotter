## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2022-11-14
##

library(tidyverse)
library(lubridate)

files <- list.files("data/",pattern = ".csv$")

all_data <- map(files, ~read_csv(here::here(paste0("data/",.))))

fresno <- all_data[[1]]

fresno <- fresno %>% 
  rename("id" = shotspotterflexid) %>% 
  mutate(beat = NA, zone = NA) %>% 
  mutate(datetime = mdy_hms(datetime)) %>% 
  mutate(date = as_date(datetime),
         time = hms::as_hms(datetime)) %>% 
  mutate(description = NA) %>% 
  select(-datetime)

oakland <- all_data[[2]] %>% 
  janitor::clean_names()

oakland <- oakland %>% 
  rename(id = objectid, description = descriptio, long = xcoord, lat = ycoord) %>% 
  mutate(date_time = mdy_hm(date_tim)) %>% 
  mutate(date = as_date(date_time),
         time = hms::as_hms(date_time)) %>% 
  mutate(city = "Oakland",
         zone = NA) %>% 
  select(-xrough, -yrough, -call_id, -date_tim, -cad, -date_time)


## note: Would be helpful to have all 911 calls
richmond <- all_data[[3]] %>% 
  janitor::clean_names()

richmond <- richmond %>% 
  rename(id = event_number, lat = latitude, long = longitude,
         description = narrative_keywords) %>% 
  select(-case_number, -labels, -total_units, -is_active, -is_forced_address,
         -cleared_date, -queue, - event_status, - dispatch_area, - primary_units, - involved_units) %>% 
  mutate(date_time = mdy_hm(received_date),.before = 1) %>% 
  mutate(date = as_date(date_time),
         time = hms::as_hms(date_time),
         city = "Richmond") 

richmond_early <- all_data[[4]]

richmond_early <- richmond_early %>% 
  mutate(city = "Richmond") %>% 
  select(-source, -zone, -cad) %>% 
  mutate(date = mdy(date))

sacramento <- all_data[[5]]

sacramento <- sacramento %>% 
  rename(id = primary_key) %>% 
  mutate(date_time = mdy_hms(paste(date_received, time_received))) %>% 
  mutate(date = as_date(date_time),
         time = hms::as_hms(date_time),
         city = "Sacramento") %>% 
  select(-date_received, - time_received, -date_time,
         -municipality, -final_case_type_translation, -report_flag)

files[[6]]
sanfran <- all_data[[6]] %>% 
  janitor::clean_names()

sanfran <- sanfran %>% 
  mutate(date = dmy(date),
         city = "San Francisco")

sanmateo <- all_data[[7]] %>% 
  janitor::clean_names()

## no exact address on this so may not be able to use.
sanmateo <- sanmateo %>% 
  mutate(date = mdy(date)) %>% 
  mutate(city = "San Mateo County") 

san_pablo <- all_data[[8]] %>% 
  janitor::clean_names()

san_pablo <- san_pablo %>%
  rename(id = respond_id ) %>% 
  mutate(date = mdy(date))

stockton <- all_data[[9]]

stockton <- stockton %>%
  rename(id = eventnumber,
         type = initialucr,
         address = eventaddress) %>% 
  mutate(date_time = mdy_hms(datetimereceived),
         date = as_date(date_time),
         time = hms::as_hms(date_time)) %>% 
  select(-date_time, - datetimereceived, -month, - year, -day,
         -yearfrac, -yearmonth, -source) 
## numrounds
## id
## time
## address
## zone
## lat
## long
## beat
## city
## state
## date