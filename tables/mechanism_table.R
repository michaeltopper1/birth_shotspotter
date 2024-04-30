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

mechanism_table <- panelsummary(six_models, 
                                caption = "\\label{mechanism_table}Effect of Gunshot Noise on Prenatal Visits (OLS)",
                                     mean_dependent = T, 
                                     stars = T,
                                    format = "latex",
                                     gof_map = gof_mapping,
                                     pretty_num = T,
                                     coef_map = c("number_gunshots"= "Gunshots")) %>% 
  add_header_above(c(" " = 1,
                     "Smoking" = 1,
                     "Visits" = 1,
                     "Delayed Care" = 1,
                     "Smoking" = 1,
                     "Visits" = 1,
                     "Delayed Care" = 1)) %>% 
  add_header_above(c(" " = 1, "Low Education" = 3, "High Education" = 3)) %>% 
  # column_spec(1, width = "8cm") %>% 
  footnote(footnotes, threeparttable = T) %>% 
  kable_styling(latex_options = "HOLD_position", font_size = 11)

writeLines(mechanism_table, "tables/mechanism_table.tex")

