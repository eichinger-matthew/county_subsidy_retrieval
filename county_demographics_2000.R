# clean demographic (race) data from census

# libraries for cleaning
library(tidyverse)
library(janitor)

# set directory
setwd("C:/Users/eichi/Desktop/us_subsidies")

# load data from census
df <- read_csv("co-est00int-sexracehisp.csv")

# documentation for dataset shows how race is coded
# only need data from year 2000
# for each county, sum pop across races to get total pop
# use pop of each county-race group with total county pop to get shares of each racial group
new <-
  df %>%
  clean_names() %>%
  mutate(race = 
           case_when(
             race == 0 ~ 'total',
             race == 1 ~ 'white',
             race == 2 ~ 'black',
             race == 3 ~ 'native_american',
             race == 4 ~ 'asian',
             race == 5 ~ 'pacific_islander',
             race == 6 ~ 'two_or_more'),
         sex = case_when(sex == 0 ~ 'total',
                         sex == 1 ~ 'male',
                         sex == 2 ~ 'female'),
         origin = case_when(origin == 0 ~ 'total',
                            origin == 1 ~ 'not_hispanic', 
                            origin == 2 ~ 'hispanic')) %>%
  select(sumlev:estimatesbase2000) %>%
  rename(popest = estimatesbase2000) %>%
  filter(sex == 'total', origin == 'total') %>%
  pivot_wider(names_from = race, values_from = popest,
              names_prefix = 'race_') %>%
  select(-c(sex, origin))


# save as rds
saveRDS(new, file = 'county_race_demogs_2000.rds')



