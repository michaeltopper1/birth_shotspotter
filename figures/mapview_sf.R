

census_block_sf %>% 
  filter(!GEOID %in% c("060759804011000",
                    "060759902000001",
                    "060759901000001",
                    "060759901000002",
                    "060759804011002")) %>% 
  left_join(sf_totals) %>% 
  mapview(zcol = "number_gunshots",
          layer.name = "Number Gunshots Detected") 

statistical_areas %>% 
  filter(NAME == "San Francisco-Oakland-Berkeley, CA") %>% 
  mapview()


sf_totals <- sf_monthly_merged %>% 
  summarize(number_gunshots = sum(number_gunshots), .by = GEOID)
