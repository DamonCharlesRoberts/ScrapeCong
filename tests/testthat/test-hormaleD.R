context("hormaleD")
library(ScrapeCongress)

context("hormaleD")
library(ScrapeCongress)
library(rtweet)
test_that('hormaleD', {
  skip_on_cran()
  token <- rtweet::get_token()
  f <- senmaleD()
  expect_message(f, "Check your Data Folder. Function ran successfully")
})
