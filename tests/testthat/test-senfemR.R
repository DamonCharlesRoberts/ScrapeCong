context("senfemR")
library(ScrapeCongress)

context("senfemR")
library(ScrapeCongress)
library(rtweet)
test_that('senfemR', {
  skip_on_cran()
  token <- rtweet::get_token()
  f <- senmaleD()
  expect_message(f, "Check your Data Folder. Function ran successfully")
})
