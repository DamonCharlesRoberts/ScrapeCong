---
title: ScrapeCong
author: Damon C. Roberts
summary: Description of the R ScrapeCong Package
---

# Set Up
```{r}
#From Github:
install.packages('devtools')
devtools::install_github('damoncharlesroberts/ScrapeCongress')
library(ScrapeCongress)
#From Cran:
install.packages('ScrapeCongress')
```
# You need to also load your Twitter Developer Credentials and API Keys into your R Script. 

To set up a twitter developer account, you must apply on their website: dev.twitter.com. Once your account has been approved, you must create an APP. Once you have created an APP, you can access your API KEY, API secret key, Access Token, and Access token Secret. Each time that you start a new R session and use the ScrapeCongress library, you will need to have your API KEY, API Secret Key, Access Token, Access Token Secret, and the Name of your APP (As it appears on twitter) handy.

You also need to create a data folder in your current working directory. The folder can be empty, just set or find your current working directory and create a folder called 'data' no need for capitalization. This is where all of your CSV's will download to. If you do not do this step, you will run into issues.

## To Start Scraping Tweets use these functions:
```{r}
senmaleD()
senfemD()
senmaleR()
senfemR()
hormaleD()
horfemD()
hormaleR()
horfemR()
```

These functions will stream the 50 most recent tweets and will download a CSV for each member of Congress and it will then create a new CSV combining all of the tweets from the same Bicameral House, Gender, and Party of the member


When running the commands, you may observe a few "Warnings" or "Errors" in the terminal (these are pretty distinct in that they mention a page not being found or not being able to access a JSON). Know that despite these, the package is still likely scraping tweets and in these instances do not indicate that there is something wrong with the package's functions. This is likely due to a twitter handle change or some privacy restrictions placed on the twitter account. Unfortunately, to my knowledge, this is just going to be an unfortunate thing that cannot be really fixed. If a MC changes their twitter handle or puts some sort of privacy restriction on, I cannot really make adjustments to that. Unless it leads to a massive loss of observations, I'll likely just suggest to treat it like a non-response in a survey. 
