library(tidyverse)

crime_data <- read_csv("analysis_data/xxcrime_geocoded.csv")

## median crime waking hours
waking_hour_median <- crime_data %>% 
  mutate(waking_hour_crime = waking_hour_crime_nonviolent + waking_hour_crime_violent) %>% 
  summarize(median(waking_hour_crime)) %>% pull()


## MENTALLY DISTURBED
sf_911 %>% 
  count(call_type_final_desc, sort = T) %>% View()
