context("hormaleD")
library(ScrapeCongress)
test_that('hormaleD', {
  skip_on_cran()
  token <- readRDS('/Users/damonroberts/.rtweet_token.rds')
  expect_message(hormaleD())
})
