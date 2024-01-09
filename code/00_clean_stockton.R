## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2023-10-03
##

library(tidyverse)

stockton <- read_csv("added_data/stockton_ca.csv")

stockton_2 <- readxl::read_excel("added_data/stockton_ca_2.xlsx") %>% 
  janitor::clean_names()


stockton <- stockton %>% 
  mutate(date = mdy_hm(datetimereceived) %>% as_date(),
          year = year(date),
          month = month(date),
          day = day(date),
          year_month = mdy(paste0(month, "-1-", year))) 

stockton_2 <- stockton_2 %>% 
  separate_wider_delim(cols = "location_address", names = c("address", "city"),
                         delim = ",", too_few = "align_end") %>% 
  mutate(city = str_trim(city)) %>% 
  mutate(city = if_else(city == "SKN", "Stockton", "San Jose")) 

stockton_2 <- stockton_2 %>% 
  mutate(state = "CA")

stockton_addresses_2 <- stockton_2 %>% 
  distinct(address, city, state)

stockton_addresses_1 <- stockton %>% 
  mutate(address = eventaddress) %>% 
  distinct(address,city,state)

stockton_addresses <- stockton_addresses_1 %>% 
  bind_rows(stockton_addresses_2)

stockton_addresses %>% 
  write_csv("address_data/stockton_addresses.csv")
  
