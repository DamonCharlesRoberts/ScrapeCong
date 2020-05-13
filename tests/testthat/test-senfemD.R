context("senfemD")
library(ScrapeCongress)
test_that('senfemD', {
  skip_on_cran()
  expect_message(horfemD())
})
test_that('senfemD', {
  skip_on_cran()
  expect_warning(senfemD)
})
