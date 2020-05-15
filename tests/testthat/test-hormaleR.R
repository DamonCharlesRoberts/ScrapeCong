context("hormaleR")
library(ScrapeCongress)
consumer_key <- readRDS('~/Desktop/RDS/consumer_key.RDS')
consumer_secret <- readRDS('~/Desktop/RDS/consumer_secret.RDS')
access_token <- readRDS('~/Desktop/RDS/access_token.RDS')
access_token_secret <- readRDS('~/Desktop/RDS/access_token_secret.RDS')
token <- setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_token_secret)
test_that('hormaleR', {
  skip_on_cran()
  testthat::expect_message(hormaleR())
})

test_that('hormaleR', {
  skip_on_cran()
  testthat::expect_warning(hormaleR())
})
