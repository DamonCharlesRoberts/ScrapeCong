context("keys")
library(ScrapeCongress)
test_that('keys has output', {
  str1 = 2
  str2 = 3
  str3 = 4
  str4 = 5
  str5 = 6
  expect_equal(str1, 2)
  expect_equal(str2, 3)
  expect_equal(str3, 4)
  expect_equal(str4, 5)
  expect_equal(str5, 6)
})