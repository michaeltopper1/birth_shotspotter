
library(tidyverse)

sst <- read_csv("data/sanfran_1622.csv")

sst <- sst %>% 
  mutate(datetime = mdy_hms(received_datetime),
         date = as_date(datetime),
         year = year(date),
         month = month(date),
         day = day(date),
         year_month = mdy(paste0(month, "-1-", year)),
         hour = hour(datetime))

sst_hourly <- sst %>% 
  count(hour) %>% 
  ggplot(aes(hour, n)) +
  geom_col() +
  scale_x_continuous(breaks = c(0, 5, 10, 15, 20), 
    labels = c("00:00", 
                                "05:00",
                                "10:00",
                                "15:00",
                                "20:00")) +
  labs(x = "Hour of the Day", y = "Number of SST Dispatches") +
  theme_minimal()

ggsave(filename = "figures/sst_hourly.jpeg", sst_hourly, 
       width = 7, height = 5)
