context("horfemR")
library(ScrapeCongress)
test_that('horfemR', {
  skip_on_cran()
  token <- rtweet::get_token()
  expect_message(horfemR())
})

