context("senmaleD")
library(ScrapeCongress)
<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
consumer_key <- readRDS('~/Desktop/RDS/consumer_key.RDS')
consumer_secret <- readRDS('~/Desktop/RDS/consumer_secret.RDS')
access_token <- readRDS('~/Desktop/RDS/access_token.RDS')
access_token_secret <- readRDS('~/Desktop/RDS/access_token_secret.RDS')
token <- setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_token_secret)
test_that('senmaleD', {
  skip_on_cran()
  testthat::expect_message(senmaleD())
})

test_that('senmaleD', {
  skip_on_cran()
  testthat::expect_warning(senmaleD())
=======
library(rtweet)
test_that('senmaleD', {
  skip_on_cran()
  token <- rtweet::get_token()
  f <- senmaleD()
  expect_message(f, "Check your Data Folder. Function ran successfully")
>>>>>>> parent of 0badd50... Final Changes before CRAN
=======
library(rtweet)
test_that('senmaleD', {
  skip_on_cran()
  token <- rtweet::get_token()
  f <- senmaleD()
  expect_message(f, "Check your Data Folder. Function ran successfully")
>>>>>>> parent of 0badd50... Final Changes before CRAN
=======
library(rtweet)
test_that('senmaleD', {
  skip_on_cran()
  token <- rtweet::get_token()
  f <- senmaleD()
  expect_message(f, "Check your Data Folder. Function ran successfully")
>>>>>>> parent of 0badd50... Final Changes before CRAN
})
