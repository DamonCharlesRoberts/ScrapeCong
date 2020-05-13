context("horfemD")
library(ScrapeCongress)
test_that('horfemD', {
  skip_on_cran()
  expect_message(horfemD())
})
test_that('horfemD', {
  skip_on_cran()
  expect_warning(horfemD)
})
