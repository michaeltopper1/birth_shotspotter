## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2022-02-05
##

library(tidyverse)
library(sf)
library(mapview)
library(patchwork)

boundaries <- st_read("data/Boundaries - Police Districts (current)/")

rollout_map <- boundaries %>% 
  mutate(dist_num = as.double(dist_num)) %>% 
  mutate(roll_2017 = ifelse(dist_num %in% c(6,7,9,10,11,15), 1, 0),
         roll_2018 = ifelse(dist_num %in% c(2,3,4,5,8,25), 1, 0), 
         roll_2020 = ifelse(dist_num %in% c(16, 17), 1, 0)) %>% 
  mutate(rollout = as.factor(case_when(
    roll_2017 == 1 ~ "2017",
    roll_2018 == 1 ~ "2018",
    roll_2020 == 1 ~ "2020"
  ))) %>% 
  ggplot() +
  geom_sf(aes(fill = rollout), alpha = 0.8) +
  # geom_sf_text(aes(label = rollout)) +
  geom_sf_text(aes(label = dist_num)) +
  ggthemes::theme_map() +
  theme(legend.key = element_rect(color = NA)) +
  labs(fill = " ", title = "Chicago Police Districts: Rollout of Shotspotter")

crimes <- read_csv("data/crimes_chicago.csv") %>% 
  janitor::clean_names()

crimes <- crimes %>% 
  mutate(date = lubridate::mdy_hms(date)) %>% 
  mutate(year = lubridate::year(date)) %>% 
  filter(year > 2009)

crimes_2017 <- crimes %>% 
  filter(year >= 2017)

crime_counts <- crimes %>% 
  mutate(district = as.double(district)) %>% 
  filter(primary_type == "HOMICIDE") %>% 
  count(district) %>% 
  mutate(district = glue::glue("District {district}")) %>% 
  mutate(district = fct_reorder(as.factor(district), n)) %>% 
  ggplot(aes(district, n)) +
  geom_col() +
  coord_flip() +
  theme_minimal() +
  labs(y = " ", x = " ", title = "Number of Homicides (2017-Present)") +
  theme(title = element_text(size = 9))

rollout_map
