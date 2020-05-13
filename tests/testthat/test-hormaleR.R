context("hormaleR")
library(ScrapeCongress)

context("hormaleR")
library(ScrapeCongress)
library(rtweet)
test_that('hormaleR', {
  skip_on_CRAN()
  token <- rtweet::get_token()
  f <- senmaleD()
  expect_message(f, "Check your Data Folder. Function ran successfully")
})