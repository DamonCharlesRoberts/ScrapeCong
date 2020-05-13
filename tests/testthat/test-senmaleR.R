context("senmaleR")
library(ScrapeCongress)
test_that('senmaleR', {
  skip_on_cran()
  expect_message(senmaleR())
})
test_that('senmaleR', {
  skip_on_cran()
  expect_warning(senmaleR())
})
