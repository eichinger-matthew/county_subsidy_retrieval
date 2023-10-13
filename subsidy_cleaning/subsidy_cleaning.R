# get and clean subsidy data
# naics classification preferred but very incomplete
# "major industry" not preferred, but more complete

# major industry method cannot be merged to trade and employment data very well (if at all)
# naics methods can be merged but, due to incompleteness, could paint an untrue story about subsidies and trade
# solution: clean data; make separate major industry and naics versions

# libraries
library(janitor)
library(tidyverse)
library(tigris)
setwd("C:/Users/eichi/Desktop/subsidy_dataset/subsidy_cleaning")

# load batch data
load("C:/Users/eichi/Desktop/subsidy_dataset/web_scraping/batch1of4_subsidy_characteristics.RData")
load("C:/Users/eichi/Desktop/subsidy_dataset/web_scraping/batch2of4_subsidy_characteristics.RData")
load("C:/Users/eichi/Desktop/subsidy_dataset/web_scraping/batch3of4_subsidy_characteristics.RData")
load("C:/Users/eichi/Desktop/subsidy_dataset/web_scraping/batch4of4_subsidy_characteristics.RData")
# get rid of "value_of_exempted_property" variable in batches 1 and 2
batch1df <- select(batch1df, -value_of_exempted_property)
batch2df <- select(batch2df, -value_of_exempted_property)
# for each batchdf, organize columns by alphabet (for easy rbinding)
batch1df <- batch1df %>% select(order(colnames(.)))
batch2df <- batch2df %>% select(order(colnames(.)))
batch3df <- batch3df %>% select(order(colnames(.)))
batch4df <- batch4df %>% select(order(colnames(.)))
# bind batch info and get rid of duplicates
df <- rbind(batch1df, batch2df, batch3df, batch4df) %>% distinct()

# clean a little bit
df_clean <-
  df %>%
  rename(naics = naics_industry_code) %>%
  filter(!is.na(county), year >= 2000) %>%
  mutate(county = ifelse(!is.na(county), str_c(county, "County", sep = " "), county),
         county = str_to_title(county),
         subsidy_value = str_remove_all(subsidy_value, "[$,]"),
         subsidy_value = as.numeric(subsidy_value),
         naics = str_remove_all(naics, "[A-Za-z]"),
         naics = str_remove_all(naics, "[-]"),
         naics = str_trim(naics)) %>%
  select(case, year, city, company, county, location, major_industry_of_parent,
         specific_industry_of_parent, naics, subsidy_value,
         type_of_subsidy, wage_data, wage_data_type, number_of_jobs_or_training_slots, 
         project_description) %>%
  left_join(fips_codes, by = c('county', 'location' = 'state_name')) %>%
  mutate(fips = str_c(state_code, county_code))

# major industries --------------------
# get total subsidies by county
countysubs <-
  df_clean %>%
  group_by(state, state_code, county, county_code, fips) %>%
  reframe(tsubs = sum(subsidy_value, na.rm = TRUE))
#write_csv(countysubs, file = "county_subsidies_after_2000.csv")
# categorize subsidies by major industry
countysubs_by_major <-
  df_clean %>%
  group_by(state, state_code, county, county_code, fips, major_industry_of_parent) %>%
  reframe(tsubs = sum(subsidy_value, na.rm = TRUE))
#write_csv(countysubs_by_major, file = "county_subsidies_by_major_industry_after_2000.csv")



# naics -------------------------
# among the many issues with the data, some obs have multiple naics
# easily identified, but need to be addressed
# split naics string into individual industries; get subsidy value in total
# given n individual naics, subsidy value for each is val/n
# add n new rows

# identify obs with multiple naics
g <- df_clean %>%
  filter(str_detect(naics, ","))
# make container
dubs <- list()
# loop over obs with multiple naics
for(i in 1:nrow(g)){
  # split string into individual naics
  s <- as.character(str_split(g$naics[i], pattern = ", ", simplify = T))
  # if check for naics being more than 1
  if(length(s) > 1){
    print("yes")
    # for each naics in set of naics
    for(j in s){
      # add a new row
      # code for relevant information in the original data
      # e.g., location, year, case, subsidy amount, etc.
      dubs[[length(dubs)+1]] <-
        data.frame(case = g$case[i], year = g$year[i],
                   location = g$location[i],
                   type_of_subsidy = g$type_of_subsidy[i],
                   subsidy_value = g$subsidy_value[i]/length(s),
                   naics = j, 
                   specific_industry_of_parent = g$specific_industry_of_parent[i],
                   major_industry_of_parent = g$major_industry_of_parent[i])
    }
  }
  # ifelse for last case
  if(i == nrow(g)){
    # unravel list into df
    temp <- do.call(rbind, dubs)
  }
}
# put new data back into original df; get rid of multi-naics obs
naics_subs <-
  df_clean %>%
  filter(!str_detect(naics, ",") | is.na(naics)) %>%
  add_row(temp)
# total subsidies by county-naics
countysubs_by_naics <-
  naics_subs %>%
  group_by(state, state_code, county, county_code, fips, naics) %>%
  reframe(tsubs = sum(subsidy_value, na.rm = TRUE))
#write_csv(countysubs_by_naics, "county_subsidies_by_sixdigit_naics_after_2000.csv")
countysubs_by_naics %>%
  count(state, naics) %>%
  arrange(desc(n)) %>% view()





