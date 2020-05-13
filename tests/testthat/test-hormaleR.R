context("hormaleR")
library(ScrapeCongress)
test_that('hormaleR', {
  skip_on_cran()
  expect_message(hormaleR())
})
test_that('hormaleR', {
  skip_on_cran()
  expect_warning(hormaleR())
})
