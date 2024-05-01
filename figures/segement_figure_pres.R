library(tidyverse)
library(ggrepel)

segment <- read_csv("figures/segment_figure.csv")


segment <- segment %>% 
  mutate(across(c("start", "end"), ~mdy(.)))

segment_figure_pres <- segment %>% 
  mutate(length = interval(start, end) / years(1) ,
         length = round(length, 2)) %>% 
  mutate(length = glue::glue("{length} years")) %>% 
  mutate(city = fct_reorder(city, desc(city))) %>% 
  ggplot() +
  geom_segment(aes(x = city, xend = city,
                   y = start, yend = end)) +
  geom_point( aes(x=city, y=start), color="dark green", alpha = 0.7, size=3,
              shape = "circle") +
  geom_point( aes(x=city, y=end), color="dark red",alpha = 0.7, size=3, shape = "square") +
  geom_text(aes(x = city, y =start, label = length), size = 3,
            vjust = 2, hjust = -.1) +
  scale_y_date(date_labels="%Y",date_breaks  ="1 year") +
  labs(x = "", y = "") +
  theme_minimal() +
  coord_flip()

## getting the average length
# segment %>% 
#   mutate(length = interval(start, end) / years(1) ,
#          length = round(length, 2)) %>% 
#   summarize(avg_length = mean(length))

ggsave(segment_figure_pres, filename = "/Users/michaeltopper/shotspotter_crime/presentations/figures_births/segment_figure_pres.jpeg", width = 7, height = 5)
