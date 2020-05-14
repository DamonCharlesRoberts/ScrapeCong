context("hormaleD")
library(ScrapeCongress)
test_that('hormaleD', {
  skip_on_cran()
  token <- rtweet::get_token()
  expect_message(hormaleD())
})
