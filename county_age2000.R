# clean county age data from census bureau

# libraries for cleaning
library(tidyverse)
library(janitor)

# load data from census bureau
df <- read_csv("co-est00int-agesex-5yr.csv")
# look at structure of df
str(df)

# only need data from year 2000
# use documentation on census bureau webpage
# shows how the agegrp variable is coded

# objective: create age categories by 20 years; 0-19, 20-39, etc. for each county
new <-
  df %>%
  clean_names() %>%
  select(sumlev:estimatesbase2000) %>%
  rename(popest = estimatesbase2000) %>%
  mutate(agegrp = case_when(agegrp == 0 ~ 'total',
                   agegrp == 1 ~ 'zero_to_four',
                   agegrp == 2 ~ 'five_to_nine',
                   agegrp == 3 ~ 'ten_to_fourteen',
                   agegrp == 4 ~ 'fifteen_to_nineteen',
                   agegrp == 5 ~ 'twenty_to_twentyfour',
                   agegrp == 6 ~ 'twentyfive_to_twentynine',
                   agegrp == 7 ~ 'thirty_to_thirtyfour',
                   agegrp == 8 ~ 'thirtyfive_to_thirtynine',
                   agegrp == 9 ~ 'forty_to_fortyfour',
                   agegrp == 10 ~ 'fortyfive_to_fortynine',
                   agegrp == 11 ~ 'fifty_to_fiftyfour',
                   agegrp == 12 ~ 'fiftyfive_to_fiftynine',
                   agegrp == 13 ~ 'sixty_to_sixtyfour',
                   agegrp == 14 ~ 'sixtyfive_to_sixtynine',
                   agegrp == 15 ~ 'seventy_to_seventyfour',
                   agegrp == 16 ~ 'seventyfive_to_seventynine',
                   agegrp == 17 ~ 'eighty_to_eightyfour',
                   agegrp == 18 ~ 'eightyfive_and_older'),
         sex = case_when(sex == 0 ~ 'total', sex == 1 ~ 'male', sex == 2 ~ 'female')) %>%
  filter(sex == 'total') %>%
  pivot_wider(names_from = agegrp, values_from = popest) %>%
  mutate(zero_to_nineteen = zero_to_four + five_to_nine + ten_to_fourteen + fifteen_to_nineteen,
         twenty_to_thirtynine = twenty_to_twentyfour + twentyfive_to_twentynine +
           thirty_to_thirtyfour + thirtyfive_to_thirtynine,
         forty_to_fiftynine = forty_to_fortyfour + fortyfive_to_fortynine +
           fifty_to_fiftyfour + fiftyfive_to_fiftynine,
         sixty_to_seventynine = sixty_to_sixtyfour + sixtyfive_to_sixtynine +
           seventy_to_seventyfour + seventyfive_to_seventynine,
         eighty_and_older = eighty_to_eightyfour + eightyfive_and_older,
         share_0_to_19 = zero_to_nineteen/total,
         share_20_to_39 = twenty_to_thirtynine/total,
         share_40_to_59 = forty_to_fiftynine/total,
         share_60_to_79 = sixty_to_seventynine/total,
         share_80_and_older = eighty_and_older/total)

# save as rds
saveRDS(new, file = 'county_ages2000.rds')



