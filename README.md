---
title: ScrapeCong
author: Damon C. Roberts
summary: Description of the R ScrapeCong Package
---

# Set Up
```{r}
install.packages('devtools')
devtools::install_github('damoncharlesroberts/ScrapeCongress')
library(ScrapeCongress)
library(readr) #having problems loading a few of the dependencies
library(rtweet)
library(dplyr)
```
# You need to also load your Twitter Developer Credentials and API Keys into your R Script. 
```{r}
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
```
To set up a twitter developer account, you must apply on their website: dev.twitter.com. Once your account has been approved, you must create an APP. Once you have created an APP, you can access your API KEY, API secret key, Access Token, and Access token Secret. 

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

To organize the tweets by line, simply run this command

```{r Tweet Per Line}
tweet_per_line()
```

When running the commands, you may observe a few "Warnings" or "Errors" in the terminal. This is likely due to a twitter handle change or some privacy restrictions placed on the twitter account. Unfortunately, to my knowledge, this is just going to be an unfortunate thing that cannot be really fixed. If a MC changes their twitter handle or puts some sort of privacy restriction on, I cannot really make adjustments to that. Unless it leads to a massive loss of observations, I'll likely just suggest to treat it like a non-response in a survey. 
