context("horfemR")
library(ScrapeCongress)

context("horfemR")
library(ScrapeCongress)
library(rtweet)
test_that('horfemR', {
  skip_on_CRAN()
  token <- rtweet::get_token()
  f <- senmaleD()
  expect_message(f, "Check your Data Folder. Function ran successfully")
})