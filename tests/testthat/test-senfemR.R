context("senfemR")
library(ScrapeCongress)
test_that('senfemR', {
  skip_on_cran()
  expect_message(senfemR())
})
test_that('senfemR', {
  skip_on_cran()
  expect_warning(senfemR())
})
