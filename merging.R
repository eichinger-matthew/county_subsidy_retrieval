# merging batch data with employment data
rm(list = ls())

# libraries
library(tidyverse)
library(janitor)
library(tigris)
# set directory
setwd("C:/Users/eichi/Desktop/us_subsidies/")
# load employment data
load("./county_info/aggregate_county_industry_employment.RData")
# load batch data
load("./web_scraping/batch1of4_subsidy_characteristics.RData")
load("./web_scraping/batch2of4_subsidy_characteristics.RData")
load("./web_scraping/batch3of4_subsidy_characteristics.RData")
# get rid of "value_of_exempted_property" variable in batches 1 and 2
batch1df <- select(batch1df, -value_of_exempted_property)
batch2df <- select(batch2df, -value_of_exempted_property)
# for each batchdf, organize columns by alphabet (for easy rbinding)
batch1df <- batch1df %>% select(order(colnames(.)))
batch2df <- batch2df %>% select(order(colnames(.)))
batch3df <- batch3df %>% select(order(colnames(.)))
# bind batch info
df <- rbind(batch1df, batch2df, batch3df)
# read county demographics
dems <- 
  read_rds("county_race_demogs_2000.rds") %>% 
  mutate(year = 2000, fips = str_c(state, county)) %>%
  select(fips, year, race_total:race_two_or_more) %>%
  rename(totalpop = race_total)
# read county ages
ages <- read_rds('county_ages2000.rds') %>%
  mutate(year = 2000, fips = str_c(state, county)) %>%
  select(fips, year, total, share_0_to_19:share_80_and_older) %>%
  rename(totalpop = total)


# minor subsidy data cleaning before merging ------------

# aggregate multi-digit NAICS industries to two-digit categories
# for each county-year-industry-subsidy, calculuate total value of subsidies received
# pivot subsidy variable (four cats) to four columns
# add new variable to measure total value of subsidies (across categories) received in a year
# use 'complete' function to fill in missing county-years

# get fips data
fips <- 
  fips_codes %>% 
  mutate(fips_full = str_c(state_code, county_code))
# aggregate industries
agg_industries <- 
  df %>%
  rename(naics = naics_industry_code) %>%
  filter(!is.na(county)) %>%
  mutate(county = ifelse(!is.na(county), str_c(county, "County", sep = " "), county),
         county = str_to_title(county),
         subsidy_value = str_remove_all(subsidy_value, "[$,]"),
         subsidy_value = as.numeric(subsidy_value),
         twodigitnum = str_sub(naics, 1, 2),
         twodigitcat = case_when(naics < 21 ~ "Forestry, fishing, hunting,and agriculturesupport",
                                 naics >= 21 & naics < 22 ~ "Mining",
                                 naics >= 22 & naics < 23 ~ "Utilities",
                                 naics >= 23 & naics < 31 ~ "Construction",
                                 naics >= 31 & naics < 42 ~ "Manufacturing",
                                 naics >= 42 & naics < 44 ~ "Wholesale trade",
                                 naics >= 44 & naics < 48 ~ "Retail trade",
                                 naics >= 48 & naics < 51 ~ "Transportation and warehousing",
                                 naics >= 51 & naics < 52 ~ "Information",
                                 naics >= 52 & naics < 53 ~ "Finance insurance",
                                 naics >= 53 & naics < 54 ~ "Real estate and rental leasing",
                                 naics >= 54 & naics < 55 ~ "Professional, scientific technical services",
                                 naics >= 55 & naics < 56 ~ "Management of companies and enterprises",
                                 naics >= 56 & naics < 61~ "Admin, support, waste management, remediation services",
                                 naics >= 61 & naics < 62 ~ "Educational services",
                                 naics >= 62 & naics < 71 ~ "Healthcare and social assistance",
                                 naics >= 71 & naics < 72 ~ "Arts, entertainment, and recreation",
                                 naics >= 72 & naics < 81 ~ "Accommodation food services",
                                 naics >= 81 & naics < 95 ~ "Other services",
                                 naics >= 95 & naics < 99 ~ "Auxiliaries",
                                 naics >= 99 ~ "Unclassified establishments"),
         twodigitcat = ifelse(is.na(twodigitcat), "unlisted", twodigitcat))
# add fips data by matching state and county names (needs both; some states have matching counties)
agg_industries_merged <- 
  agg_industries %>% 
  left_join(fips, by = c('county' = 'county', 'location' = 'state_name')) %>%
  filter(!is.na(fips_full))
# make dataset of county-year-industry-subsidies
cyr_industry_subs <-
  agg_industries_merged %>%
  filter(type_of_subsidy != "") %>%
  rename(fips = fips_full) %>%
  group_by(state, state_code, county, fips, year, twodigitcat, type_of_subsidy) %>%
  summarise(annual_sub = sum(subsidy_value, na.rm = TRUE)) %>%
  pivot_wider(id_cols = c(fips, county, year, twodigitcat),
              names_from = "type_of_subsidy", values_from = "annual_sub") %>%
  clean_names() %>%
  ungroup()
# to use 'complete', need to have a list of variables that should take 0 or na val when missing
# to avoid typing out the list, use basic loop
cnames <- colnames(cyr_industry_subs)[5:ncol(cyr_industry_subs)]
lst <- list()
for(i in cnames){
  lst[[i]] <- 0
}
# use 'lst' as fill argument in complete
# note that you LOSE the county variable here, which is good - too many county names are not unique
cyr_industry_subs_complete <-
  cyr_industry_subs %>%
  mutate(across(c(grant:tax_exemption), ~ifelse(is.na(.x), 0, .x)),
         year = as.numeric(year),
         twodigitcat = as.factor(twodigitcat)) %>%
  complete(fips, year = 1998:2021, twodigitcat, fill = lst) %>%
  ungroup()

# minor employment data cleaning before merging ----------------

# unlist counties
empdf <- do.call(rbind, agg_dfs)
# calculate share payroll/employment ratio
empdf <-
  empdf %>%
  mutate(ratio = share_payroll/share_employment, 
         year = as.numeric(year),
         twodigitcat = as.factor(twodigitcat))
# use complete to fill in missing years
empdf_comp <-
  empdf %>%
  group_by(fips) %>%
  complete(year = 1998:2021, twodigitcat) %>%
  mutate(across(c(employees:share_payroll), ~ifelse(is.na(.x), 0, .x))) %>%
  ungroup()


# merge employment and subsidy dataframes --------------------

# master dataset
master <-
  cyr_industry_subs_complete %>%
  left_join(empdf_comp, by = c('fips', 'year','twodigitcat')) %>%
  left_join(dems, by = c("fips", "year")) %>%
  left_join(ages, by = c('fips', 'year', 'totalpop')) %>%
  select(-county) %>%
  left_join(fips, by = c('fips' = 'fips_full')) %>%
  select(year, state, state_code, county, fips, everything())

# save
saveRDS(master, file = "county_subsidies_and_employment.rds")



