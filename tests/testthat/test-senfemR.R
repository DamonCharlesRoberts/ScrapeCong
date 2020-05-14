context("senfemR")
library(ScrapeCongress)
test_that('senfemR', {
  skip_on_cran()
  token <- rtweet::get_token()
  expect_message(senfemR())
})

