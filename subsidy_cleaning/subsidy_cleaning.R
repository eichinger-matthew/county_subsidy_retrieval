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
  mutate(county = 
           case_when(
             !is.na(county) & location != "Louisiana" ~ 
               str_c(county, "County", sep = " "),
             !is.na(county) & location == "Louisiana" ~ 
               str_c(county, "Parish", sep = " "),
             TRUE ~ NA
           ),
         county = str_to_title(county), # make sure this comes BEFORE the next one
         county = ifelse(str_detect(county, "Mcc"), str_replace(county, "Mcc", "McC"), county),
         county = ifelse(str_detect(county, "Mcd"), str_replace(county, "Mcd", "McD"), county),
         county = ifelse(str_detect(county, "St "), str_replace(county, "St ", "St. "), county),
         county = ifelse(str_detect(county, "G'ville"), "Greenville County", county),
         county = ifelse(str_detect(county, "Fond Du"), str_replace(county, "Du", "du"), county),
         county = ifelse(location == "Louisiana" & str_detect(county, "Desoto"),
                         str_replace(county, "Desoto", "De Soto"), county),
         county = ifelse(location == "Louisiana" & str_detect(county, "St. John The"),
                         "St. John the Baptist Parish", county),
         county = ifelse(location == "Maryland" & str_detect(county, "Saint Mary's"),
                         "St. Mary's County", county),
         county = ifelse(str_detect(county, "Lewis &Amp; Clark County"),
                         "Lewis and Clark County", county),
         county = ifelse(str_detect(county, "Baltimore City|Baltimore County County"),
                         "Baltimore County", county),
         subsidy_value = str_remove_all(subsidy_value, "[$,]"),
         subsidy_undisclosed = ifelse(subsidy_value == "undisclosed", 1, 0),
         subsidy_value = as.numeric(subsidy_value),
         naics = str_remove_all(naics, "[A-Za-z]"),
         naics = str_remove_all(naics, "[-]"),
         naics = str_trim(naics)) %>%
  select(case, year, city, company, county, location, major_industry_of_parent,
         specific_industry_of_parent, naics, subsidy_value, subsidy_undisclosed,
         type_of_subsidy, wage_data, wage_data_type, number_of_jobs_or_training_slots, 
         project_description) %>%
  left_join(fips_codes, by = c('county', 'location' = 'state_name')) %>%
  mutate(fips = str_c(state_code, county_code))


# clean naics and county names -------------------------
# among the many issues with the data, some obs have multiple naics
# easily identified, but need to be addressed
# split naics string into individual industries; get subsidy value in total
# given n individual naics, subsidy value for each is val/n
# add n new rows

# identify obs with multiple naics
g <- df_clean %>% filter(str_detect(naics, ","))
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
                   subsidy_undisclosed = g$subsidy_undisclosed[i],
                   subsidy_value = g$subsidy_value[i]/length(s),
                   naics = j,
                   major_industry_of_parent = g$major_industry_of_parent[i],
                   specific_industry_of_parent = g$specific_industry_of_parent[i],
                   wage_data = g$wage_data[i],
                   wage_data_type = g$wage_data_type[i],
                   number_of_jobs_or_training_slots = g$number_of_jobs_or_training_slots[i],
                   state_code = g$state_code[i],
                   state = g$state[i],
                   county = g$county[i],
                   county_code = g$county_code[i],
                   fips = g$fips[i])
    }
  }
  # ifelse for last case
  if(i == nrow(g)){
    # unravel list into df
    ntemp <- do.call(rbind, dubs)
  }
}


# ----------- same thing, for counties

# same thing, but for counties
# identify obs with multiple naics
h <- df_clean %>% 
  filter(str_detect(county, ",")) %>%
  mutate(county = str_remove_all(county, "County|and|And|Others|Multiple"),
         county = str_trim(county))
# make container
hdubs <- list()
# loop over obs with multiple naics
for(i in 1:nrow(h)){
  # split string into individual naics
  s <- as.character(str_split(h$county[i], pattern = ", ", simplify = T))
  # if check for naics being more than 1
  if(length(s) > 1){
    print("yes")
    # for each naics in set of naics
    for(j in s){
      # add a new row
      # code for relevant information in the original data
      # e.g., location, year, case, subsidy amount, etc.
      hdubs[[length(hdubs)+1]] <-
        data.frame(case = h$case[i], 
                   year = h$year[i],
                   location = h$location[i],
                   county = j,
                   type_of_subsidy = h$type_of_subsidy[i],
                   subsidy_undisclosed = h$subsidy_undisclosed[i],
                   subsidy_value = h$subsidy_value[i]/length(s),
                   naics = h$naics[j],
                   major_industry_of_parent = h$major_industry_of_parent[i],
                   specific_industry_of_parent = h$specific_industry_of_parent[i],
                   wage_data = h$wage_data[i],
                   wage_data_type = h$wage_data_type[i],
                   number_of_jobs_or_training_slots = h$number_of_jobs_or_training_slots[i]) %>%
        mutate(county = str_c(county, "County", sep = " "),
               county = str_trim(county)) %>%
        left_join(fips_codes, by = c('location' = 'state_name', 'county'))
      # join with fips
      
    }
  }
  # ifelse for last case
  if(i == nrow(h)){
    # unravel list into df
    ctemp <- do.call(rbind, hdubs)
  }
}


# put new data back into original df; get rid of multi-naics obs
naics_subs <-
  df_clean %>%
  filter(!str_detect(naics, ",") | is.na(naics)) %>%
  filter(!str_detect(county, ",|and|And|Multiple|Others|Various|Statewide")) %>%
  add_row(ntemp) %>%
  add_row(ctemp)




# naics -----------------------------
# total subsidies by county-naics
# remove na values on fips to avoid later merging headaches
countysubs_by_naics <-
  naics_subs %>%
  group_by(state, state_code, county, location, county_code, fips, naics) %>%
  reframe(tsubs = sum(subsidy_value, na.rm = TRUE)) %>%
  drop_na(fips)
#write_csv(countysubs_by_naics, "county_subsidies_by_sixdigit_naics_after_2000.csv")



# major industries --------------------
# get total subsidies by county
countysubs <-
  naics_subs %>%
  group_by(state, state_code, county_code, fips) %>%
  reframe(tsubs = sum(subsidy_value, na.rm = TRUE))
#write_csv(countysubs, file = "county_subsidies_after_2000.csv")
# categorize subsidies by major industry
countysubs_by_major <-
  naics_subs %>%
  group_by(state, state_code, county_code, fips, major_industry_of_parent) %>%
  reframe(tsubs = sum(subsidy_value, na.rm = TRUE))
#write_csv(countysubs_by_major, file = "county_subsidies_by_major_industry_after_2000.csv")




