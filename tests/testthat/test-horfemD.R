context("horfemD")
consumer_key <- readRDS('~/Desktop/RDS/consumer_key.RDS')
consumer_secret <- readRDS('~/Desktop/RDS/consumer_secret.RDS')
access_token <- readRDS('~/Desktop/RDS/access_token.RDS')
access_token_secret <- readRDS('~/Desktop/RDS/access_token_secret.RDS')
token <- setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_token_secret)
library(ScrapeCongress)

context("horfemD")
library(ScrapeCongress)
library(rtweet)
test_that('horfemD', {
  skip_on_cran()
<<<<<<< HEAD
<<<<<<< HEAD
  testthat::expect_message(horfemD())
})

test_that('horfemD', {
  skip_on_cran()
  testthat::expect_warning(horfemD())
=======
  token <- rtweet::get_token()
  f <- senmaleD()
  expect_message(f, "Check your Data Folder. Function ran successfully")
>>>>>>> parent of 0badd50... Final Changes before CRAN
=======
  token <- rtweet::get_token()
  f <- senmaleD()
  expect_message(f, "Check your Data Folder. Function ran successfully")
>>>>>>> parent of 0badd50... Final Changes before CRAN
})
