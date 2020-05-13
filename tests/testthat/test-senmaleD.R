context("senmaleD")
library(ScrapeCongress)
test_that('senmaleD', {
  skip_on_cran()
  expect_message(horfemD())
})
test_that('senmaleD', {
  skip_on_cran()
  expect_warning(senmaleD)
})
