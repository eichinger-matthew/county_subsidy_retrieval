# merging batch data with employment data

# libraries
library(tidyverse)
library(janitor)
library(tigris)
# set directory
setwd("C:/Users/eichi/Desktop/us_subsidies")
# load employment data
load("./county_info/aggregate_county_industry_employment.RData")
# load batch data
load("batch1of4_subsidy_characteristics.RData")
load("batch2of4_subsidy_characteristics.RData")
# bind batch info
df <- rbind(batch1df, batch2df)
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
  mutate(county = 
           ifelse(!is.na(county), str_c(county, "County", sep = " "), county),
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
                                 naics >= 99 ~ "Unclassified establishments"))
# add fips data by matching state and county names (needs both; some states have matching counties)
agg_industries <- 
  agg_industries %>% 
  left_join(fips, by = c('county' = 'county', 'location' = 'state_name'))
# make dataset of county-year-industry-subsidies
cyr_industry_subs <-
  agg_industries %>%
  filter(type_of_subsidy != "", !is.na(county)) %>%
  mutate(fips = str_c(state_code, county_code)) %>%
  group_by(fips, county, year, twodigitcat, type_of_subsidy) %>%
  summarise(annual_sub = sum(subsidy_value, na.rm = TRUE)) %>%
  pivot_wider(id_cols = c(fips, county, year, twodigitcat),
              names_from = "type_of_subsidy", values_from = "annual_sub") %>%
  clean_names() %>%
  ungroup()
# to use 'complete', need to have a list of variables that should take 0 or na val when missing
# to avoid typing out the list, use basic loop
cnames <- colnames(cyr_industry_subs)[5:26]
lst <- list()
for(i in cnames){
  lst[[i]] <- 0
}
# use 'lst' as fill argument in complete
cyr_industry_subs <-
  cyr_industry_subs %>%
  mutate(across(c(grant:tax_credit_rebate_2), ~ifelse(is.na(.x), 0, .x)),
         year = as.numeric(year)) %>%
  group_by(fips, county) %>%
  complete(year = 1977:2021, twodigitcat,
           fill = lst)



# minor employment data cleaning before merging ----------------

# unlist counties
empdf <- do.call(rbind, agg_dfs)
# calculate share payroll/employment ratio
empdf <-
  empdf %>%
  mutate(ratio = share_payroll/share_employment, 
         year = as.numeric(year),
         twodigitcat = factor(twodigitcat, levels = c(unique(twodigitcat))))
# use complete to fill in missing years
empdf_comp <-
  empdf %>%
  group_by(fips) %>%
  complete(year = 1998:2021, twodigitcat) %>%
  mutate(across(c(employees:share_payroll), ~ifelse(is.na(.x), 0, .x)))
# join fips data to employment data
empdf_comp <-
  empdf_comp %>%
  left_join(fips, by = c('fips' = 'fips_full'))



# merge employment and subsidy dataframes --------------------

# master dataset
master <-
  empdf_comp %>%
  left_join(cyr_industry_subs, by = c('year', 'fips', 'county', 'twodigitcat')) %>%
  left_join(dems, by = c("fips", "year")) %>%
  left_join(ages, by = c('fips', 'year', 'totalpop')) %>%
  select(year, fips, county, everything())

# save
saveRDS(master, file = "county_subsidies_and_employment.rds")











