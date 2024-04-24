library(tidyverse)
library(readxl)
library(geomtextpath)
files <- list.files("analysis_data/", pattern = "_blocks.csv$")
files <- files[-5]

cities <- read_excel("added_data/all_cities_restrictions.xlsx") %>% 
  mutate(across(starts_with("year"), ~as_date(.)))

cities <- cities %>% 
  mutate(city = if_else(city == "Oakland 1", "Oakland", city))


gunshots <- map_df(files, ~read_csv(paste0("analysis_data/", .)))

gunshots <- gunshots %>% 
  mutate(year = year(year_month)) %>% 
  filter(year < 2021)


gunshots <- gunshots %>% 
  left_join(cities, join_by(shotspotter_city == city))

gunshots <- gunshots %>% 
  group_by(shotspotter_city) %>% 
  filter(year_month >= year_start_inclusive & year_month <= year_end_inclusive) %>% 
  ungroup()

gunshots_all <- gunshots %>% 
  summarize(number_gunshots = sum(number_gunshots), .by = year_month)

gunshots_cities <- gunshots %>% 
  summarize(number_gunshots = sum(number_gunshots), .by = c(year_month, shotspotter_city))

# 
# trends <- gunshots_cities %>% 
#   ggplot(aes(year_month, number_gunshots)) +
#   geom_line() +
#   geom_point(size = .5, alpha = 0.8) +
#   facet_wrap(~shotspotter_city, scales = "free") +
#   labs(y = "Gunshot Detections (Monthly)", x = "") +
#   theme_minimal()


trends <- gunshots %>% 
  rowwise() %>% 
  mutate(number_gunshot_non_working = number_gunshots - number_gunshot_working_hours) %>% 
  ungroup() %>% 
  summarize(number_gunshots = sum(number_gunshots),
            number_gunshot_nonworking = sum(number_gunshot_non_working),
            number_gunshot_working = sum(number_gunshot_working_hours),
            .by = c(year_month, shotspotter_city)) %>% 
  pivot_longer(cols = starts_with("number"), names_to = "gunshot_type",
               values_to = "number_gunshots") %>% 
  mutate(gunshot_type = case_when(
    gunshot_type == "number_gunshot_nonworking" ~ "Non-Working Hours",
    gunshot_type == "number_gunshot_working" ~ "Working Hours",
    gunshot_type == "number_gunshots" ~ "Total Gunshots"
  )) %>% 
  mutate(gunshot_type = factor(gunshot_type, c("Total Gunshots", "Working Hours", "Non-Working Hours"))) %>% 
  ggplot(aes(year_month, number_gunshots, color = gunshot_type,
             linetype = gunshot_type)) +
  geom_line() +
  facet_wrap(~shotspotter_city, scales = "free") +
  labs(color = "Gunshot Time", x = "", y = "Number Gunshots Detected",
       linetype = "Gunshot Time") +
  ggthemes::scale_color_stata() +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave(trends, filename = "figures/trends.jpeg",
       width = 7, height = 5)

