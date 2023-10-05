# script for scraping subsidy events from Subsidy Tracker
# see their terms of service; NO REQUEST-BOMBING THEIR DATABASE
# script makes use of RSelenium to navigate pages

# libraries
library(rvest) # for scraping
library(tidyverse) # for nice language
library(tigris) # for fips identifiers
library(RSelenium) # for navigating

# get state codes from fips
states <- tigris::fips_codes %>%
  filter(state_code <= 58, state_name != "District of Columbia") %>%
  distinct(state) %>%
  pull()

# make results container
res <- list()
# loop over states
for(i in 1:length(states)){
  # prepare list object for each state
  # sublists for a dataset of info and webpages for more info on subsidies
  res[[states[i]]] <- list("datasets" = list(), "webpages" = list())
}



# main loop -----------------------

# loop does the following things:
# navigate to main page of a state
# parse html of main state page

# run if-else condition to see if num. of subsidies span more than one webpage
# if they do, then navigate to the final webpage to see how many pages you need to loop through
# loop thru webpages; for each page, get the html table of subsidies and their urls
# clean up table and urls; store in container

# define main url for scraping
url <- "https://subsidytracker.goodjobsfirst.org/?company_op=starts&state="

# main loop
# LOOP STOPPED ---- BEGIN AT 29
for(j in 1:length(states)){
  # paste url string with state extension
  main_page <- paste0(url, states[j])
  # navigate to main state page
  remDr$navigate(main_page)
  # pause to give server time
  Sys.sleep(5)
  
  # navigate to main page and parse html
  html_obj <- remDr$getPageSource(header = TRUE)[[1]] %>% read_html()
  
  # ----- get page numbers
  # get number of li elements
  # html_obj %>%
  #   html_nodes("li") %>% as.character() %>%
  #   .grepl("pager-item", .)
  #   html_nodes(".pager-item active")
  
  # if-else condition for number of entries
  len_pages <- html_obj %>% html_nodes("li") %>% as.character() %>% length(.)
  # if num entries less than 30, do nothing (too few to care about)
  if(len_pages < 30){
    
  } else{ # if num entires more than 30, find last page num
    # go to last arrow thing on page
    remDr$findElement(using = "xpath", '//*[@id="contentResult"]/div/div[2]/ul/li[12]')$clickElement()
  
    # pause to give system time
    Sys.sleep(2)
  
    # get last page
    final_num <- remDr$findElement(using = "xpath", '//*[@id="contentResult"]/div/div[2]/ul/li[12]')
    num_pages <- unlist(final_num$getElementText())
    # print number of pages as a check on progress
    print(num_pages)
  
    # navigate back to main state page
    remDr$goBack()
    # sleep for server
    Sys.sleep(2)
  
  # given number of pages for a state, loop over pages
  for(i in 1:num_pages){
    # ----- start scraping each page, then navigate to next
    # if first page, just read html
    if(i == 1){
      info <- remDr$getPageSource(header = TRUE)[[1]] %>% read_html()
    } else{
      # if not first page, paste together url using page num and state name
      # extension page
      extension <- paste("&page=", i, sep = "")
      full_page <- paste(main_page, extension, sep = "")
      # navigate to new page
      remDr$navigate(full_page)
      # server sleep
      Sys.sleep(2)
      # get html of webpage
      info <- remDr$getPageSource(header = TRUE)[[1]] %>% read_html()
    }
    # get table html
    tab <- 
      info %>%
      html_table() %>%
      .[[2]]
    # print to see it is running
    print(tab)
    # get urls
    urls <- info %>% 
      html_nodes("table") %>% 
      .[2] %>%
      html_nodes("tr") %>% 
      html_nodes("a") %>% 
      html_attr("href") %>%
      .[!grepl("parent", .)] %>% 
      .[grepl("subsidy-tracker", .)]
    print(urls)
    # assign to list
    res[[states[j]]]$datasets[[i]] <- tab
    res[[states[j]]]$webpages[[i]] <- urls
  }
  }
}
# save results
save(res, file = "subsidy_tracker_scrape_results.RData")


