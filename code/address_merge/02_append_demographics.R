## this file will get all of the census data needed to merge to 
## census blocks (Census 2020) or block group (ACS) 
## blocks are the lowest level and cannot have income attached
## blocks get data from deccenial census (2020)
##block-groups get data from the ACS. This is the lowest level
## in which income data can be found due to privacy reasons
library(tidyverse)
library(tidycensus)

mother_addresses <- read_csv("analysis_data/addresses_censusblocks.csv")

## https://www2.census.gov/programs-surveys/decennial/2020/technical-documentation/complete-tech-docs/summary-file/2020Census_PL94_171Redistricting_StatesTechDoc_English.pdf
## page 113 gives a good summary of the variables


# getting decennial census data -------------------------------------------

## this data gives race attributes at the block level

## explore variables
variables <- load_variables(2020, "pl")

## variables to download
download_variables <- c(total_pop = "P1_001N",
                        pop_one_race = "P1_002N",
                        one_race_white = "P1_003N",
                        one_race_black = "P1_004N",
                        one_race_asian = "P1_006N",
                        two_or_more_race = "P1_009N",
                        total_hispanic = "P2_002N")


# getting by county -------------------------------------------------------

sf_race <- get_decennial(geography = "block",
              state = "CA",
              county = "San Francisco",
              variables = download_variables,
              geometry = F) %>% 
  pivot_wider(names_from = "variable", values_from = "value") 

fresno_race <- get_decennial(geography = "block",
                         state = "CA",
                         county = "Fresno",
                         variables = download_variables,
                         geometry = F) %>% 
  pivot_wider(names_from = "variable", values_from = "value") 


stockton_race <- get_decennial(geography = "block",
                         state = "CA",
                         county = "San Joaquin County",
                         variables = download_variables,
                         geometry = F) %>% 
  pivot_wider(names_from = "variable", values_from = "value") 


oakland_race <- get_decennial(geography = "block",
                              state = "CA",
                              county = "Alameda County",
                              variables = download_variables,
                              geometry = F) %>% 
  pivot_wider(names_from = "variable", values_from = "value") 


pablo_richmond_race <- get_decennial(geography = "block",
                                     state = "CA",
                                     county = "Contra Costa County",
                                     variables = download_variables,
                                     geometry = F) %>% 
  pivot_wider(names_from = "variable", values_from = "value") 


east_palo_race <- get_decennial(geography = "block",
                                state = "CA",
                                county = "San Mateo County",
                                variables = download_variables,
                                geometry = F) %>% 
  pivot_wider(names_from = "variable", values_from = "value") 

sd_race <- get_decennial(geography = "block",
                         state = "CA",
                         county = "San Diego County",
                         variables = download_variables,
                         geometry = F) %>% 
  pivot_wider(names_from = "variable", values_from = "value") 


# getting income data -----------------------------------------------------
## comes from ACS - median household income in past 12 months 2016-2020 5-year survey
ca_income_sf <- get_acs(
  geography = "tract", 
  variables = "B19013_001",
  state = "CA",
  county = "San Francisco",
  year = 2020
) %>% mutate(county = "San Francisco")

ca_income_fresno <- get_acs(
  geography = "tract", 
  variables = "B19013_001",
  state = "CA",
  county = "Fresno",
  year = 2020
) %>% mutate(county = "Fresno")

ca_income_stockton <- get_acs(
  geography = "tract", 
  variables = "B19013_001",
  state = "CA",
  county = "San Joaquin County",
  year = 2020
) %>% mutate(county = "San Joaquin County")

ca_income_oakland <- get_acs(
  geography = "tract", 
  variables = "B19013_001",
  state = "CA",
  county = "Alameda County",
  year = 2020
) %>%  mutate(county = "Alameda County")

ca_income_pablo <- get_acs(
  geography = "tract", 
  variables = "B19013_001",
  state = "CA",
  county = "Contra Costa County",
  year = 2020
) %>% mutate(county = "Contra Costa County")

ca_income_east_palo <- get_acs(
  geography = "tract", 
  variables = "B19013_001",
  state = "CA",
  county = "San Mateo County",
  year = 2020
) %>% mutate(county = "San Mateo County")

ca_income_sd <- get_acs(
  geography = "tract", 
  variables = "B19013_001",
  state = "CA",
  county = "San Diego County",
  year = 2020
) %>% mutate(county = "San Diego County")

ca_income <- ca_income_sf %>% 
  bind_rows(ca_income_fresno, ca_income_oakland,
            ca_income_east_palo, ca_income_stockton,
            ca_income_pablo, ca_income_sd)

ca_income <- ca_income %>% 
  extract(NAME, into = "census_tract", "Census Tract\\s(.{1,10}),\\s.{1,},.{1,}",
          remove = F)

## some tracts are in two counties. This aggregates by mean
ca_income_aggregate <- ca_income %>% 
  group_by(census_tract, variable) %>% 
  summarize(estimate = mean(estimate, na.rm = T)) %>% 
  ungroup()

ca_income_aggregate <- ca_income_aggregate %>% 
  mutate(census_tract = as.double(census_tract))

ca_income_aggregate <- ca_income_aggregate %>% 
  mutate(med_income = estimate) %>% 
  select(-variable, -estimate)


# combining race data -----------------------------------------------------

race_data <- east_palo_race %>% 
  bind_rows(fresno_race, oakland_race, pablo_richmond_race, sf_race, stockton_race, sd_race)

race_data <- race_data %>% 
  mutate(across(starts_with("one_race"), ~100 * ./pop_one_race, .names = "percent_{.col}")) %>% 
  mutate(percent_hispanic = 10 * total_hispanic/total_pop)

race_data <- race_data %>% 
  extract(NAME, into = c("census_block", "census_block_group", "census_tract"),
          "Block\\s(\\d{1,4}).{1,}Group\\s(\\d{1,2}),.{1,}Tract\\s(.{1,}\\d{1,3}),.{1,}", remove = F) 

ca_income_race <- ca_income %>% 
  filter(census_tract %in% race_data$census_tract) 


mother_addresses <- mother_addresses %>% 
  left_join(race_data) 

ca_income <- ca_income %>% 
  extract(NAME, into = c("census_tract", "census_county", "state"),
          "Census Tract\\s(.{1,}),\\s(.{1,}),\\s(.{1,})", remove = F)

mother_address <- mother_addresses %>% 
  extract(NAME, into = c("block", "block_group",
                         "census_tract", "county", "state"),
          "Block\\s(.{1,}),\\sBlock Group\\s(.{1,}),\\sCensus Tract\\s(.{1,}),\\s(.{1,}),\\s(.{1,})",
          remove = F)


# combining income data ---------------------------------------------------

mother_address <- mother_address %>% 
  mutate(census_tract = as.double(census_tract)) %>% 
  left_join(ca_income_aggregate)



mother_address %>% 
  write_csv("analysis_data/addresses_census_blocks_append_demos.csv")
