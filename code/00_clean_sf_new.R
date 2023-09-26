library(tidyverse)
library(sf)


# loading in data ---------------------------------------------------------

sf <- read_csv("data/sanfran_1622.csv")


# adding sunrise/sunset to sf ---------------------------------------------

sf <- sf %>% 
  mutate(across(ends_with("datetime"), ~mdy_hms(.))) %>% 
  mutate(date_shot = as_date(received_datetime)) %>% 
  rename(datetime_shot = received_datetime) 




## getting the intersection points: lat and long from the sf data into separate columns
sf <- sf %>% 
  separate(intersection_point,
           into = c("point", "latitude", "longitude"),
           sep = "\\s",
           remove = F) %>% 
  mutate(across(starts_with("l"), ~. %>% str_replace("\\(", "") %>% 
                  str_replace("\\)", "")))


# getting only necessary columns ------------------------------------------

sf <- sf %>% select(cad_number,
              datetime_shot, date_shot,
              latitude, longitude,
              call_type_original, police_district)



sf %>% 
  write_csv("created_data/sf_16_22_clean.csv")



## using the same crs as the census block data.
sf <- sf %>% 
  st_as_sf(coords = c("latitude", "longitude"), crs = st_crs(census_block_sf))


## after doing this, 412 of 8242 (5%) of shots were matched into multiple census blocks.
## not a big deal, but may want to drop these? Note that CAD number is distinct

# sf_joined <- st_join(sf, census_block_sf, join = st_within)
# 
# 
# ## creating panel from 2016-2022
# 
# panel_dates <- seq(as_date("2016-01-01"), as_date("2023-01-01") , by= "day") %>% 
#   as_tibble() %>% 
#   rename(date = value)
# 
# 
# sf_dates <- sf_joined %>% 
#   add_count(cad_number) %>% 
#   relocate(n) %>% 
#   mutate(duplicate_area = ifelse(n > 1, 1, 0 ), .before = 1) %>% 
#   distinct(cad_number, .keep_all = T) %>% 
#   select(-n) %>% 
#   mutate(across(ends_with("datetime"), ~mdy_hms(.))) %>% 
#   mutate(received_date = as_date(received_datetime),
#          onscene_date = as_date(onscene_datetime), .before = 1)
# 
# ## need to connect all addresses into their respective blocks and then can merge together.
# panel_dates %>% 
#   left_join(sf_dates, by = join_by(date == received_date)) %>% 
#   rename(census_block = NAME) %>% 
#   count(date, census_block, gunshot_daylight, sort = T)
# 

