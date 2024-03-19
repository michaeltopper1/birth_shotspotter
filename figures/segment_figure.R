library(tidyverse)
library(ggrepel)

segment <- read_csv("figures/segment_figure.csv")


segment <- segment %>% 
  mutate(across(c("start", "end"), ~mdy(.)))

segement_figure <- segment %>% 
  mutate(length = interval(start, end) / years(1) ,
         length = round(length, 2)) %>% 
  mutate(length = glue::glue("{length} years")) %>% 
  mutate(city = fct_reorder(city, desc(city))) %>% 
  ggplot() +
  geom_segment(aes(x = city, xend = city,
                   y = start, yend = end)) +
  geom_point( aes(x=city, y=start), color="grey", alpha = 0.7, size=3 ) +
  geom_point( aes(x=city, y=end), color="black",alpha = 0.7, size=3 ) +
  geom_text(aes(x = city, y =start, label = length), size = 3,
            vjust = 2) +
  scale_y_date(date_labels="%Y",date_breaks  ="1 year") +
  labs(x = "", y = "") +
  theme_minimal() +
  coord_flip()
