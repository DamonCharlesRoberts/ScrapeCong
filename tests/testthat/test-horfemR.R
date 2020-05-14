context("horfemR")
library(ScrapeCongress)
test_that('horfemR', {
  skip_on_cran()
  token <- readRDS('/Users/damonroberts/.rtweet_token.rds')
  expect_message(horfemR())
})

