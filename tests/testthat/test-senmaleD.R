context("senmaleD")
library(ScrapeCongress)
test_that('senmaleD', {
  skip_on_cran()
  token <- readRDS('/Users/damonroberts/.rtweet_token.rds')
  expect_message(senmaleD())
})
