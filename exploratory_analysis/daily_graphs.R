## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2022-01-11
##

library(tidyverse)
library(lubridate)
library(kableExtra)


ss_chicago <- read_csv("data/shotspot_chicago.csv") %>% janitor::clean_names()

start_date <- ymd("2017-01-01")
end_date <- ymd("2022-01-01")

## clean the data
ss_chicago <- ss_chicago %>% 
  filter(!is.na(district)) %>% 
  mutate(date = lubridate::mdy_hms(date)) %>% 
  arrange(date) %>% 
  group_by(district) %>% 
  complete(date = seq(start_date, end_date, by = 'days')) %>% 
  mutate(shot_spotted = ifelse(is.na(unique_id), 0, 1)) %>% 
  separate(date, c("date", "time_occurred"), sep = " ") %>% 
  mutate(year = year(date), month = month(date), day = day(date))

ss_chicago %>% 
  group_by(date, district) %>% 
  summarize(shots_day = sum(shot_spotted)) %>% 
  group_by(district) %>% 
  summarize(avg_shots_per_day = mean(shots_day, na.rm = T)) %>% 
  ungroup() %>% mutate(district = paste0("District ", district)) %>% 
  mutate(district = fct_reorder(as.character(district), avg_shots_per_day)) %>% 
  ggplot(aes(district, avg_shots_per_day)) +
  geom_col() +
  coord_flip()+
  theme_minimal()

ss_chicago %>% 
  group_by(date, district) %>% 
  summarize(shots_day = sum(shot_spotted)) %>% 
  group_by(district) %>% 
  summarize(avg_shots_per_day = mean(shots_day, na.rm = T)) %>% 
  ungroup() %>% mutate(district = paste0("District ", district)) %>% 
  arrange(desc(avg_shots_per_day)) %>% 
  kbl(col.names = c("District", "Average Shots Per-Day")) %>% 
  kable_styling()
  
ss_chicago %>% 
  group_by(district, year, month) %>% 
  summarize(num_shots = sum(shot_spotted)) %>% 
  mutate(date = ymd(paste(year, month, "1", "-"))) %>% 
  mutate(district = glue::glue("District {district}")) %>% 
  ggplot(aes(date, num_shots)) +
  geom_line() +
  facet_wrap(~district, scales = "free_y") +
  labs(y = "Number of Shorts Per-Month", x = "") +
  theme_minimal()


