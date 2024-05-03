library(tidyverse)
library(sf)
library(tigris)

cities <- read_csv("figures/cal_cities_lat_long.csv") %>% 
  janitor::clean_names()


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


cities <- cities %>% 
  st_as_sf(coords = c( "longitude", "latitude"), crs = st_crs(ca_counties)) 

cities <- cities %>% 
  filter(name %in% c("Bakersfield",  "East Palo Alto", "Fresno", "Oakland", "Richmond",
                     "San Francisco", "San Diego", "San Pablo", "Stockton" ))


ca_map_presentation <- ca %>% 
  ggplot() +
  geom_sf(fill = "white", color = "grey") +
  geom_sf(data = ca_counties, alpha = 0.2,
          linewidth = 0.2,
          fill = "light grey") +
  geom_sf(data = ca_counties_used, fill = "darkred",
          color = "white",
          linewidth = 0.2,
          alpha = 0.8) +
  geom_sf(data = cities) +
  labs(title = "The Nine Sample Cities in California",
       subtitle = "Counties are marked in red. Cities are black points.") +
  ggsflabel::geom_sf_label_repel(data = cities, aes(label = name),
                                 size = 4.7, max.overlaps = 20 ) +
  ggthemes::theme_map() +
  theme(plot.title = element_text(size=18))

ggsave(ca_map_presentation, filename = "/Users/michaeltopper/shotspotter_crime/presentations/figures_births/ca_map_presentation.jpeg", width = 5, height = 7)
