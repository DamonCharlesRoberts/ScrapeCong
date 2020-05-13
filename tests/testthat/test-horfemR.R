context("horfemR")
library(ScrapeCongress)
test_that('horfemR', {
  skip_on_cran()
  expect_message(horfemD())
})
test_that('horfemR', {
  skip_on_cran()
  expect_warning(horfemR)
})
