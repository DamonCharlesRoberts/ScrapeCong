context("senfemD")
library(ScrapeCongress)
<<<<<<< HEAD
consumer_key <- readRDS('~/Desktop/RDS/consumer_key.RDS')
consumer_secret <- readRDS('~/Desktop/RDS/consumer_secret.RDS')
access_token <- readRDS('~/Desktop/RDS/access_token.RDS')
access_token_secret <- readRDS('~/Desktop/RDS/access_token_secret.RDS')
token <- setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_token_secret)
test_that('senfemD', {
  skip_on_cran()
  testthat::expect_message(senfemD())
})

test_that('senfemD', {
  skip_on_cran()
  testthat::expect_warning(senFemD())
=======

context("senfemD")
library(ScrapeCongress)
library(rtweet)
test_that('senfemD', {
  skip_on_cran()
  token <- rtweet::get_token()
  f <- senmaleD()
  expect_message(f, "Check your Data Folder. Function ran successfully")
>>>>>>> parent of 0badd50... Final Changes before CRAN
})
