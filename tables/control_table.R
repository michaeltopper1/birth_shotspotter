library(tidyverse)
library(panelsummary)
library(kableExtra)
library(fixest)

sst <- read_csv("analysis_data/oakland_gunshots_1_blocks.csv")

sst <- sst %>%
  mutate(fake_outcome = rnorm(n()))

model <- sst %>% 
  feols(fake_outcome ~ number_gunshots | census_block + year_month)

six_models <- list(model, model, model, model, model, model)

gof_mapping <- tribble(~raw, ~clean, ~fmt,
                       "nobs", "Observations", 0,
                       "FE: census_block", "FE: Census Block", 3,
                       "FE: year_month", "FE: Year by Month", 3)

footnotes <- map(list("* p < 0.1, ** p < 0.05, *** p < 0.01",
                      "Add in all footnotes here."), ~str_remove_all(., "\n"))

control_table <- panelsummary(six_models, 
                                     caption = "\\label{control_table}Effect of Gunshot Noise on Controls (OLS)",
                                     mean_dependent = T, 
                                     stars = T,
                                     gof_map = gof_mapping,
                                     pretty_num = T,
                                     coef_map = c("number_gunshots"= "Gunshots"),
                                     format = "latex") %>% 
  add_header_above(c(" " = 1,
                     "Male Infant" = 1,
                     "First-time Mother" = 1,
                     "Mother's Age" = 1,
                     "Hispanic" = 1,
                     "Bachelors or Higher" = 1,
                     "Black" = 1)) %>% 
  column_spec(1, width = "5cm") %>% 
  footnote(footnotes, threeparttable = T) %>% 
  kable_styling(latex_options = "HOLD_position", font_size = 11)

writeLines(control_table, "tables/control_table.tex")

