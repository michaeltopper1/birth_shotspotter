library(tidyverse)
library(panelsummary)
library(kableExtra)
library(fixest)
library(modelsummary)

sst <- read_csv("analysis_data/oakland_gunshots_1_blocks.csv")

sst <- sst %>%
  mutate(fake_outcome = rnorm(n()))



summary_stats <- datasummary(
    (`Very Preterm` = hp) +
    (`Very Low Birthweight` = mpg) +
    (`Age` = hp) +
    (`Bachelors or Higher` = mpg) +
    (`Hispanic` = hp) +
    (`Asian` = mpg) +
    (`Black` = hp) +
    (`White` = mpg) +
    (`First time Mother` = hp) +
    (`Male Infant` = mpg) 
  ~ Mean + (Std.Dev. = SD) + (Min = Min) + (Max = Max),
  data = mtcars,
  output = "data.frame",
  escape = F)

summary_stats_pres <- summary_stats %>% 
  kbl(booktabs = T,
      format = "html") %>% 
  kable_classic(full_width = T, html_font = "Cambria") %>% 
  group_rows(group_label = "Panel A: Birth Outcomes", 1, 2,
  ) %>% 
  group_rows(group_label = "Panel B: Controls", 3 , 10,
             latex_gap_space = "0.3cm") %>% 
  column_spec(1, width = "8cm") %>% 
  footnote("Total number of observations is 38,373.",threeparttable = T)


writeLines(summary_stats_pres, "/Users/michaeltopper/shotspotter_crime/presentations/tables_births/summary_stats_pres.html")
