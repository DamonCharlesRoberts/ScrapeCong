context("senfemD")
library(ScrapeCongress)
test_that('senfemD', {
  skip_on_cran()
  token <- readRDS('/Users/damonroberts/.rtweet_token.rds')
  expect_message(senfemD())
})
