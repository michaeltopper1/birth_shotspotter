
gunshots_sf <- sf_joined %>% 
  filter(!GEOID %in% c("060759804011000",
                       "060759902000001",
                       "060759901000001",
                       "060759901000002",
                       "060759804011002")) %>% 
  st_drop_geometry() %>% 
  group_by(GEOID) %>% 
  summarize(number_gunshots = n()) 


quantile(gunshots_sf$number_gunshots, c(.25, .5, .75, 1))

census_block_sf %>% 
  left_join(gunshots_sf) %>% 
  mapview()

census_block_sf %>% 
  filter(!GEOID %in% c("060759804011000",
                    "060759902000001",
                    "060759901000001",
                    "060759901000002",
                    "060759804011002")) %>%  
  mutate()
  ggplot() +
  geom_sf(aes(fill = value))
  # left_join(sf_totals) %>% 
  mapview(zcol = "value",
          layer.name = "Number Gunshots Detected") 

statistical_areas %>% 
  filter(NAME == "San Francisco-Oakland-Berkeley, CA") %>% 
  mapview()


sf_totals <- sf_monthly_merged %>% 
  summarize(number_gunshots = sum(number_gunshots), .by = GEOID)
