# script for scraping detailed subsidy data using urls scraped from previous script
# routine: navigate to each url, grab info and store
# issue: function very quick; but elapsed time is long - how to solve?
# consequence: scraping every url takes around 30 hours on an rtx2070; do in batches

# subsidy analysis
library(tidyverse)
library(janitor)
library(rvest)
library(RSelenium)

# load scraped subsidy data from previous script
load("subsidy_tracker_scrape_results.RData")

# get subsidy urls for each state
# store in container
state_store <- list()
for(i in seq_along(res)){
  # if state has data in list...
  if(length(res[[i]]$webpages) > 0){
  # unlist urls
  state_store[[i]] <- unlist(res[[i]]$webpages)
  } # if not, do nothing; alaska has no data, throws an annoying null vector
}
# unlist all pages from list to get massive vector of urls
all_pages <- unlist(state_store)

# scraping ---------------------------------

# write function to get features of each subsidy
get_pages <- function(x){
  # if an error pops up when reading a webpage, just return NA; program stops otherwise
  tryCatch(
    error = function(cnd){
      return(NA)},
    # read webpage if no error
    {page <- read_html(x)
    # from webpage, get variable names as "keys"
    keys <- page %>%
      html_nodes('div') %>%
      html_nodes('b') %>%
      html_text2()
    # get variable values as "vals" and format
    vals <- page %>%
      html_nodes('div') %>%
      html_nodes(xpath = '/html/body/div/div/div/text()') %>%
      as.character() %>%
      .[grepl(":", .)] %>%
      .[1:length(.)]
    # if condition for whether num keys and vals is same
    if(length(keys) == length(vals)){
      # if same, then put them into df
      df <- 
        data.frame(cbind(keys, vals)) %>%
        mutate(vals = str_remove(vals, ":"),
               vals = str_trim(vals))
      # return dataframe of subsidy characteristics
      return(df)
    }else{# if num keys and vals not equal for some reason
      # return NA
      return(NA)
      }
    }
  )
}
# how long does this take to run? it is quick; but elapsed time is long. oh well
system.time(get_pages(x = all_pages[5]))
# around 300,000 webpages; each page takes 0.5 seconds (upper bound), so 2 pages per second
# 150,000 seconds total; (150,000/60)/60 comes out to about 42 hours total runtime if looped
# even when vectorized, still takes enormous time (but around 30 hours)


# since I need my computer during day, i ran the script at night
# I ran in 1/4 batches, each one about 8 hours
# make sure you do not have to interrupt your comp; if you stop program, nothing is saved

# ----- batch 1

# get urls from 1 to (1/4)*length of url vector
batch1 <- all_pages[1:floor(length(all_pages)/4)]
# apply get_pages() function to first batch
batch1res <- 
  batch1 %>%
  map(get_pages)
# get rid of NA observations from first batch
# these are cases where num keys and values are not equal
batch1res_nomissing <- batch1res[!is.na(batch1res)]
# to identify unique cases for later, loop over list and add case identifier
# could also use rowid, which would be faster
for(i in 1:length(batch1res_nomissing)){
  batch1res_nomissing[[i]] <- 
    batch1res_nomissing[[i]] %>%
    mutate(case = i)
}
# unlist the dataframes
# use rbind (rowbind) to concatenate
# pivot the "keys" (i.e., variable) to columns
# clean names with janitor() package
batch1df <- 
  do.call(rbind, batch1res_nomissing) %>%
  pivot_wider(names_from = "keys", values_from = "vals") %>%
  clean_names()

# SAVE THE RESULT PLEASE
# save(batch1df, file = "batch1of4_subsidy_characteristics.RData")

# get count of missing industries out of curiosity
batch1df %>%
  count(major_industry_of_parent) %>%
  arrange(desc(n))


# REPEAT ABOVE FOR NEXT THREE QUARTERS OF URLs

# ---- batch 2
batch2 <- all_pages[(floor(length(all_pages)/4)+1):floor(length(all_pages)/2)]
# apply
batch2res <- 
  batch2 %>%
  map(get_pages)
# get rid of na
batch2res_nomissing <- batch2res[!is.na(batch2res)]
for(i in 1:length(batch2res_nomissing)){
  batch2res_nomissing[[i]] <- 
    batch2res_nomissing[[i]] %>%
    mutate(case = i)
}
# pivot keys to be columns
batch2df <- do.call(rbind, batch2res_nomissing) %>%
  pivot_wider(names_from = "keys", values_from = "vals") %>%
  clean_names()
# save
# save(batch2df, file = "batch2of4_subsidy_characteristics.RData")

# get count of missing industries
batch2df %>%
  count(major_industry_of_parent) %>%
  arrange(desc(n))


# ---- batch 3
batch3 <- all_pages[(floor(length(all_pages)/2)+1):(3*floor(length(all_pages)/4))]
# apply
batch3res <- 
  batch3 %>%
  map(get_pages)
# get rid of na
batch3res_nomissing <- batch3res[!is.na(batch3res)]
for(i in 1:length(batch3res_nomissing)){
  batch3res_nomissing[[i]] <- 
    batch3res_nomissing[[i]] %>%
    mutate(case = i)
}
# pivot keys to be columns
batch3df <- do.call(rbind, batch3res_nomissing) %>%
  pivot_wider(names_from = "keys", values_from = "vals") %>%
  clean_names()
# save
# save(batch3df, file = "batch3of4_subsidy_characteristics.RData")

# get count of missing industries
batch3df %>%
  count(major_industry_of_parent) %>%
  arrange(desc(n))


# ---- batch 4
batch4 <- all_pages[(3*floor(length(all_pages)/4)+1):length(all_pages)]
# apply
batch4res <- 
  batch4 %>%
  map(get_pages)
# get rid of na
batch4res_nomissing <- batch4res[!is.na(batch4res)]
for(i in 1:length(batch4res_nomissing)){
  batch4res_nomissing[[i]] <- 
    batch4res_nomissing[[i]] %>%
    mutate(case = i)
}
# pivot keys to be columns
batch4df <- do.call(rbind, batch4res_nomissing) %>%
  pivot_wider(names_from = "keys", values_from = "vals") %>%
  clean_names()
# save
save(batch4df, file = "batch4of4_subsidy_characteristics.RData")

# get count of missing industries
batch4df %>%
  count(major_industry_of_parent) %>%
  arrange(desc(n))













