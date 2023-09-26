
library(tidyverse)

sst <- read_csv("data/sanfran_1622.csv")

sst <- sst %>% 
  mutate(datetime = mdy_hms(received_datetime),
          date = as_date(datetime),
          year = year(date),
          month = month(date),
          day = day(date),
          year_month = mdy(paste0(month, "-1-", year)))



# districts with SST ------------------------------------------------------

district_sst_trend <- sst %>% 
  filter(police_district %in% c("BAYVIEW", "INGLESIDE","MISSION",
                                "NORTHERN", "SOUTHERN", "TENDERLOIN")) %>% 
  group_by(police_district, year_month, year) %>% 
  filter(year > 2015 & year < 2023) %>% 
  summarize(n = n()) %>% 
  ggplot(aes(year_month, n)) +
  geom_line() +
  facet_wrap(~police_district) +
  labs(y = 'Number of Monthly SST Dispatches', x = "") +
  theme_minimal()

ggsave("figures/sst_district_trend.jpeg", plot = district_sst_trend,
       width = 7, height = 5)


# districts that don't have SST in SF -------------------------------------

sst %>% 
  filter(!police_district %in% c("BAYVIEW", "INGLESIDE","MISSION",
                                "NORTHERN", "SOUTHERN", "TENDERLOIN")) %>% 
  group_by(police_district, year_month, year) %>% 
  filter(year > 2015 & year < 2023) %>% 
  summarize(n = n()) %>% 
  ggplot(aes(year_month, n)) +
  geom_line() +
  facet_wrap(~police_district) +
  labs(y = 'Number of SST Dispatches', x = "") +
  theme_minimal()
