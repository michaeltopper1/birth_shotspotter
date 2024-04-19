library(tidyverse)
library(panelsummary)
library(kableExtra)
library(fixest)

sst <- read_csv("analysis_data/oakland_gunshots_blocks_1.csv")

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



table_main <- panelsummary(six_models, 
             mean_dependent = T, 
             stars = T,
             gof_map = gof_mapping,
             pretty_num = T,
             coef_map = c("number_gunshots"= "Gunshots")) %>% 
  add_header_above(c(" " = 1, 
                     "Pooled" = 1,
                     "Low Education" = 1, 
                     "High Education" = 1,
                     "Pooled" = 1,
                     "Low Education" = 1, 
                     "High Education" = 1)) %>% 
  add_header_above(c(" " = 1,
                     "Very Low Birth Weight" = 3,
                     "Very Premature" = 3)) %>% 
    footnote(footnotes, threeparttable = T) %>% 
    kable_styling(latex_options = "HOLD_position", font_size = 11)

seven_models <- list(model, model, model, model, model,
                     model,model)

main_table <- panelsummary(seven_models, 
             seven_models,
             panel_labels = c("Panel A: Very Low Birth Weight",
                              "Panel B: Very Preterm"),
             caption = "\\label{main_table}Effect of Gunshot Noise on Very Low Birth Weight and Very Preterm (OLS)",
             mean_dependent = T, 
             stars = T,
             gof_map = gof_mapping,
             pretty_num = T,
             coef_map = c("number_gunshots"= "Gunshots"),
             collapse_fe =  T,
             hline_after = T,
             italic = T,
             format = "latex") %>% 
  add_header_above(c(" " = 1, 
                     "All Mothers" = 1,
                     "Low Education" = 1,
                     "High Education" = 1,
                     "Low Education" = 1, 
                     "High Education" = 1,
                     "Low Education" = 1, 
                     "High Education" = 1)) %>% 
  add_header_above(c(" " = 4,
                     "Working Hours" = 2,
                     "Non-Working Hours" = 2)) %>% 
  footnote(footnotes, threeparttable = T) %>% 
  kable_styling(latex_options = "HOLD_position", font_size = 10)

writeLines(main_table, "tables/main_table.tex")

