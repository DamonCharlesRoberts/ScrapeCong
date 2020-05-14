context("senfemD")
library(ScrapeCongress)
test_that('senfemD', {
  skip_on_cran()
  token <- rtweet::get_token()
  expect_message(senfemD())
})
