library(tidyverse)
library(sf)
library(tigris)


ca <- states(year = 2020) %>% 
  filter(NAME == "California") 

ca_counties <- counties(state = "CA",
                        year = 2020)


counties <- c("Alameda",
              "Contra Costa",
              "Fresno",
              "Kern",
              "San Francisco",
              "San Diego",
              "San Joaquin",
              "San Mateo")

ca_counties_used <- ca_counties %>% 
  filter(NAME %in% counties)


ca_county_figure <- ca %>% 
  ggplot() +
  geom_sf(fill = "white", color = "grey") +
  geom_sf(data = ca_counties, alpha = 0.2,
          linewidth = 0.2,
          fill = "light grey") +
  geom_sf(data = ca_counties_used, fill = "darkred",
          color = "white",
          linewidth = 0.2,
          alpha = 0.8) +
  ggsflabel::geom_sf_label_repel(data = ca_counties_used, aes(label = NAME),
                                 size = 2.3) +
  ggthemes::theme_map()


