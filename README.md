---
title: ScrapeCong
author: Damon C. Roberts
summary: Description of the R ScrapeCong Package
---

# Set Up
```{r eval=FALSE}
## INSTALL FROM GITHUB

install.packages('devtools')
devtools::install_github('damoncharlesroberts/ScrapeCongress')
library(ScrapeCongress)

## INSTALL FROM CRAN
install.packages('ScrapeCongress')
library(ScrapeCongress)
```
# You need to also load your Twitter Developer Credentials and API Keys into your R Script. 
```{r eval=FALSE}
api_key <- "YOUR API KEY HERE"
api_secret <- "YOUR API SECRET KEY HERE"
access_token <- "YOUR ACCESS TOKEN HERE"
access_token_secret <- "YOUR ACCESS TOKEN SECRET HERE"
token <- create_token(
  app = "APP NAME HERE",
  consumer_key = api_key,
  consumer_secret = api_secret,
  access_token = access_token,
  access_secret = access_token_secret
)
token <- retweet::get_token() # This pulls the token environment up to make sure that the functions grab the most recent token created.
```
To set up a twitter developer account, you must apply on their website: dev.twitter.com. Once your account has been approved, you must create an APP. Once you have created an APP, you can access your API KEY, API secret key, Access Token, and Access token Secret. 

You also need to set up a data folder in your working directory. The files that you download need to go somewhere, create a 'data' (case sensitive) folder in the same working directory that your RStudio instance is in. 

## To Start Scraping Tweets use these functions:
```{r eval=FALSE}
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

# Example Code
```{r eval=FALSE}
install.packages('devtools')
devtools::install_github('damoncharlesroberts/ScrapeCongress', force = TRUE)
library(ScrapeCongress)
api_key <- "API_KEY"
api_secret <- "API_SECRET"
access_token <- "ACCESS_TOKEN"
access_token_secret <- "ACCESS_TOKEN_SECRET"
token <- create_token(
  app = "APP NAME",
  consumer_key = api_key,
  consumer_secret = api_secret,
  access_token = access_token,
  access_secret = access_token_secret
)
token <- get_token()
senmaleD()
senfemD()
senfemR()
senmaleR()
horfemD()
horfemR()
hormaleD()
hormaleR()
```
