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
                      "Standard errors are clustered by census-block. 
                      Very Low Birth Weight is defined as a birth below 1,500 grams.
                      Working Hours defined as 9:00am-5:00pm.
                      High Education defined as bachelor's or higher."), ~str_remove_all(., "\n"))



seven_models <- list(model, model, model, model, model,
                     model,model)

main_table <- panelsummary(seven_models,
                           mean_dependent = T, 
                           stars = T,
                           gof_map = gof_mapping,
                           pretty_num = T,
                           coef_map = c("number_gunshots"= "Gunshots"),
                           collapse_fe =  T,
                           hline_after = T,
                           italic = T,
                           format = "html") %>% 
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
  kable_classic(full_width = T, html_font = "Cambria") 

main_table_col1 <- main_table %>% 
  column_spec(2, background = "yellow")

main_table_col23 <- main_table %>% 
  column_spec(3, background = "yellow") %>% 
  column_spec(4, background = "lightgreen")

main_table_col47 <- main_table %>% 
  column_spec(5, background = "yellow") %>% 
  column_spec(6, background = "yellow") %>% 
  column_spec(7, background = "lightgreen") %>% 
  column_spec(8, background = "lightgreen")


writeLines(main_table, "/Users/michaeltopper/shotspotter_crime/presentations/tables_births/main_table_bw.html")

writeLines(main_table, "/Users/michaeltopper/shotspotter_crime/presentations/tables_births/main_table_bw_col1.html")

