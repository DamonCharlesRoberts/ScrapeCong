context("hormaleD")
library(ScrapeCongress)
test_that('hormaleD', {
  skip_on_cran()
  expect_message(hormaleD())
})
test_that('hormaleD', {
  skip_on_cran()
  expect_warning(hormaleD())
})
