context("senmaleD")
library(ScrapeCongress)
test_that('senmaleD', {
  skip_on_cran()
  token <- rtweet::get_token()
  expect_message(senmaleD())
})
