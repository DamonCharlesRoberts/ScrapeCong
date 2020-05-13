context("horfemD")
library(ScrapeCongress)

context("horfemD")
library(ScrapeCongress)
library(rtweet)
test_that('horfemD', {
  skip_on_CRAN()
  token <- rtweet::get_token()
  f <- senmaleD()
  expect_message(f, "Check your Data Folder. Function ran successfully")
})