context("senmaleR")
library(ScrapeCongress)

test_that('senmaleR', {
  skip_on_cran()
  token <- rtweet::get_token()
  expect_message(senmaleR())
})
