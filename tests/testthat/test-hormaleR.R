context("hormaleR")
library(ScrapeCongress)
test_that('hormaleR', {
  skip_on_cran()
  token <- rtweet::get_token()
  expect_message(hormaleR())
})
