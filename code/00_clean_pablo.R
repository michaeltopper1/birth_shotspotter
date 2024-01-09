## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2023-10-03
##

library(tidyverse)

pablo <- readxl::read_excel("added_data/pablo.xlsx") %>% janitor::clean_names()

pablo <- pablo %>% 
  distinct(respond_id, .keep_all = T)

## changing from stupid excel time to regular time
pablo <- pablo %>% 
  mutate(time = hms::as_hms(time))

pablo 