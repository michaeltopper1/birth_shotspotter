## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2023-03-24
##

library(tidyverse)

sf_crime <- read_csv("data/sf_incidents_18.csv") %>% 
  janitor::clean_names()

sf_crime_2 <- read_csv("data/sf_incidents_03.csv") %>% 
  janitor::clean_names()


sf_crime <- sf_crime %>% 
  filter(incident_year < 2021)

sf_crime_2 <- sf_crime_2 %>% 
  mutate(date = mdy(date))

sf_crime_2 <- sf_crime_2 %>% 
  mutate(incident_year = year(date))

sf_crime_2 <- sf_crime_2 %>% 
  filter(incident_year > 2015)

sf_crime_2 <- sf_crime_2 %>% 
  relocate(incident_year) %>% 
  select(1:15) 

sf_crime <- sf_crime %>% 
  select(incident_date,
         incident_time, incident_year, incident_id,
         incident_number,
         incident_code, incident_subcategory, incident_description,
         latitude, longitude, point) 

sf_crime_2 <- sf_crime_2 %>% 
  select(incident_year, incidnt_num, incident_code, category,
         descript, date, time, x, y, location, pd_id) %>% 
  rename(incident_description = descript,
         incident_number = incidnt_num,
         incident_time = time,
         incident_subcategory = category,
         latitude = y,
         longitude = x,
         point = location,
         incident_id = pd_id,
         incident_date = date) 

sf_total <- sf_crime %>% mutate(incident_number = as.character(incident_number)) %>% 
  bind_rows(sf_crime_2)

sf_total %>% 
  write_csv("created_data/sf_incidents_ungeocoded.csv")
