context("hormaleR")
library(ScrapeCongress)
test_that('hormaleR', {
  skip_on_cran()
  token <- readRDS('/Users/damonroberts/.rtweet_token.rds')
  expect_message(hormaleR())
})
