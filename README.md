# local_subsidy_analysis
Analysis of government subsidizing patterns for manufacturing industries across US.<br />
 
This project is ongoing. My long term goal is to study why some counties chose to abandon manufacturing when the tides of globalization shifted strongly against them in the 1990s while others did not. This is an important question for several reasons. First, as the analysis shows, major subsidies in manufacturing did not influence the manufacturing share of employment between 2000 and 2019. It just kept old industries barely alive. This suggests the money spent on subsidies was a waste (of course, we will never know what would have happened had counties not subsidized old industry). Second, government subsidies for old industries only delays people from switching to new, more efficient and profitable industries. People know they can stay in their old job and be protected by the government, which acts as an incentive for them not to learn new skills. Third, subsidies crowd out private investment. If firms know they will have to compete for labor against a government-subsidized industry, then they have less of an incentive to invest new capital and technology in a county. The price of labor will be artificially high - demand will be propped up artificially by the subsidized industry.<br />

A key model in this study is not in this repository. The model is a voter-level decision model that analyzes the choice of an individual to work in a new industry or an old industry. The benefit of the new industry is a higher wage and a higher chance of employment. The cost is the time to learn new skills and the psychological loss from leaving an industry that forms the basis of your identity, both of which depend on the age of an individual. I combine this with a simple model of electoral competition to study the conditions under which a politician invests (i.e., subsidizes) in the new industry or the old industry. I show that this decision is endogenous to the age of county - having more young people makes it easier for a county to invest in new industries because young people have not accumulated a mental stock in an old industry, and they learn new skills faster. I use the data in this repository to test the insight.

 
# Note

The data in this repository was gathered from the US Census Bureau and scraped from Subsidy Tracker. Subsidy Tracker is a project by Good Jobs First, a watchdog organization that records cases of wasteful subsidies and corporate misconduct by governments and firms. Please see their [terms of service](https://goodjobsfirst.org/terms-of-service/) to see how you can and cannot scrape their webpages. A key point to note is that you *cannot* scrape information off their webpage if it throttles their servers. At a minimum, make sure you use sleep timers between your pulls to ease traffic on their side.


# File Organization

There are four "main" files in this repository. They fall into webscraping, merging, and analysis categories.<br />

subsidy_scrape_basic_data.R is a script that runs a webscraping program. It retrieves basic subsidy information from Subsidy Tracker, including a url page containing more detailed information about a subsidy. It primarily runs on the `RSelenium` package.<br />

subsidy_scrape_detailed_data.R is a script that extracts subsidy information using the url pages from the basic scrape program. It contains more detailed information about each subsidy. It takes around 30 hours to run, so make sure you have available time.<br />

merging.R is used to join dataframes at the county-year level. It joins age, demographic, and industry employment data from the Census Bureau with subsidy data from Subsidy Tracker. Once fully merged, each observation (row) is a subsidy instance at the county-year level, and it has variables (columns) related to county demographics, ages, and industry employment levels.<br />

subsidy_analysis.R does some basic data cleaning, visualization, and statistical modeling. Nothing publishable, just some exploratory digging.





