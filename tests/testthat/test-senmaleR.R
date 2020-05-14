context("senmaleR")
library(ScrapeCongress)

test_that('senmaleR', {
  skip_on_cran()
  token <- readRDS('/Users/damonroberts/.rtweet_token.rds')
  expect_message(senmaleR())
})
