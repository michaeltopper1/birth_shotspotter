library(tidyverse)
library(panelsummary)
library(kableExtra)
library(fixest)
library(modelsummary)

sst <- read_csv("analysis_data/oakland_gunshots_blocks_1.csv")

sst <- sst %>%
  mutate(fake_outcome = rnorm(n()))



summary_stats <- datasummary(
  (`Very Preterm` = hp) +
    (`Very Low Birthweight` = mpg) +
    (`Gestation Length (weeks)` = hp) +
    (`Preterm` = mpg) +
    (`Low Birthweight` = hp) +
    (`Birthweight (grams)` = mpg) +
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


summary_stats <- summary_stats %>% 
  kbl(booktabs = T,
      caption = "\\label{summary_stats}Summary Statistics",
      format = "latex") %>% 
  kable_styling(latex_options = "HOLD_position", font_size = 11) %>% 
  group_rows(group_label = "Panel A: Birth Outcomes", 1, 6,
  ) %>% 
  group_rows(group_label = "Panel B: Controls", 7 , 14,
             latex_gap_space = "0.3cm") %>% 
  column_spec(1, width = "8cm") %>% 
  footnote("Add footnotes here",threeparttable = T)


writeLines(summary_stats, "tables/summary_stats.tex")
