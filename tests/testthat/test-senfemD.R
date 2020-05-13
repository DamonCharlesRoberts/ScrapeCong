context("senfemD")
library(ScrapeCongress)

context("senfemD")
library(ScrapeCongress)
library(rtweet)
test_that('senfemD', {
  skip_on_cran()
  token <- rtweet::get_token()
  f <- senmaleD()
  expect_message(f, "Check your Data Folder. Function ran successfully")
})
