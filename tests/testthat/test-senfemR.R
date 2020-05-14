context("senfemR")
library(ScrapeCongress)
test_that('senfemR', {
  skip_on_cran()
  token <- readRDS('/Users/damonroberts/.rtweet_token.rds')
  expect_message(senfemR())
})

