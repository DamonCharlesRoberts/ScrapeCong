context("senmaleR")
library(ScrapeCongress)

context("senmaleR")
library(ScrapeCongress)
library(rtweet)
test_that('senmaleR', {
  skip_on_CRAN()
  token <- rtweet::get_token()
  f <- senmaleD()
  expect_message(f, "Check your Data Folder. Function ran successfully")
})
