library(tidyverse)

files <- list.files("analysis_data/", pattern = "_blocks.csv$")

gunshots <- map_df(files, ~read_csv(paste0("analysis_data/", .)))

gunshots_all <- gunshots %>% 
  summarize(number_gunshots = sum(number_gunshots), .by = year_month)

gunshots_cities <- gunshots %>% 
  summarize(number_gunshots = sum(number_gunshots), .by = c(year_month, shotspotter_city))


gunshots_cities %>% 
  ggplot(aes(year_month, number_gunshots)) +
  geom_line() +
  facet_wrap(~shotspotter_city, scales = "free") +
  labs(y = "Gunshot Detections (Monthly)", x = "") +
  theme_minimal()
