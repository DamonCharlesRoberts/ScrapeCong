library(testthat)
library(ScrapeCongress)


test_that("senmaleD gets tweets", {
  skip_on_cran()

  n <- 50
  token <- readRDS("twitter_tokens")
  x <- get_timeline(c("@SenDougJones"), n = n, token = token)
  expect_true(is.data.frame(x))
  expect_named(x)
  expect_true("status_id" %in% names(x))
  expect_gt(nrow(x), 100)
  expect_gt(ncol(x), 25)
  xts <- ts_data(x, by = "days")
  expect_true(is.data.frame(xts))
  p <- ts_plot(xts)
  #expect_true(inherits(p, "ggplot"))
  #if (requireNamespace("ggplot2", quietly = TRUE)) {
  g <- readRDS("g.rds")
  p <- ts_plot(g, "hours", trim = 1)
  expect_true(inherits(p, "ggplot"))
  d <- ts_data(g, "hours", trim = 1)
  expect_true(is.data.frame(d))
  #expect_equal(ncol(d), 4)
  unlink("Rplots.pdf")
})

test_that("senfemD gets tweets", {
  skip_on_cran()

  n <- 50
  token <- readRDS("twitter_tokens")
  x <- get_timeline(c("@SenDougJones"), n = n, token = token)
  expect_true(is.data.frame(x))
  expect_named(x)
  expect_true("status_id" %in% names(x))
  expect_gt(nrow(x), 100)
  expect_gt(ncol(x), 25)
  xts <- ts_data(x, by = "days")
  expect_true(is.data.frame(xts))
  p <- ts_plot(xts)
  #expect_true(inherits(p, "ggplot"))
  #if (requireNamespace("ggplot2", quietly = TRUE)) {
  g <- readRDS("g.rds")
  p <- ts_plot(g, "hours", trim = 1)
  expect_true(inherits(p, "ggplot"))
  d <- ts_data(g, "hours", trim = 1)
  expect_true(is.data.frame(d))
  #expect_equal(ncol(d), 4)
  unlink("Rplots.pdf")
})

test_that("senfemR gets tweets", {
  skip_on_cran()

  n <- 50
  token <- readRDS("twitter_tokens")
  x <- get_timeline(c("@SenDougJones"), n = n, token = token)
  expect_true(is.data.frame(x))
  expect_named(x)
  expect_true("status_id" %in% names(x))
  expect_gt(nrow(x), 100)
  expect_gt(ncol(x), 25)
  xts <- ts_data(x, by = "days")
  expect_true(is.data.frame(xts))
  p <- ts_plot(xts)
  #expect_true(inherits(p, "ggplot"))
  #if (requireNamespace("ggplot2", quietly = TRUE)) {
  g <- readRDS("g.rds")
  p <- ts_plot(g, "hours", trim = 1)
  expect_true(inherits(p, "ggplot"))
  d <- ts_data(g, "hours", trim = 1)
  expect_true(is.data.frame(d))
  #expect_equal(ncol(d), 4)
  unlink("Rplots.pdf")
})

test_that("senmaleR gets tweets", {
  skip_on_cran()

  n <- 50
  token <- readRDS("twitter_tokens")
  x <- get_timeline(c("@SenDougJones"), n = n, token = token)
  expect_true(is.data.frame(x))
  expect_named(x)
  expect_true("status_id" %in% names(x))
  expect_gt(nrow(x), 100)
  expect_gt(ncol(x), 25)
  xts <- ts_data(x, by = "days")
  expect_true(is.data.frame(xts))
  p <- ts_plot(xts)
  #expect_true(inherits(p, "ggplot"))
  #if (requireNamespace("ggplot2", quietly = TRUE)) {
  g <- readRDS("g.rds")
  p <- ts_plot(g, "hours", trim = 1)
  expect_true(inherits(p, "ggplot"))
  d <- ts_data(g, "hours", trim = 1)
  expect_true(is.data.frame(d))
  #expect_equal(ncol(d), 4)
  unlink("Rplots.pdf")
})

test_that("horfemR gets tweets", {
  skip_on_cran()

  n <- 50
  token <- readRDS("twitter_tokens")
  x <- get_timeline(c("@SenDougJones"), n = n, token = token)
  expect_true(is.data.frame(x))
  expect_named(x)
  expect_true("status_id" %in% names(x))
  expect_gt(nrow(x), 100)
  expect_gt(ncol(x), 25)
  xts <- ts_data(x, by = "days")
  expect_true(is.data.frame(xts))
  p <- ts_plot(xts)
  #expect_true(inherits(p, "ggplot"))
  #if (requireNamespace("ggplot2", quietly = TRUE)) {
  g <- readRDS("g.rds")
  p <- ts_plot(g, "hours", trim = 1)
  expect_true(inherits(p, "ggplot"))
  d <- ts_data(g, "hours", trim = 1)
  expect_true(is.data.frame(d))
  #expect_equal(ncol(d), 4)
  unlink("Rplots.pdf")
})

test_that("horfemD gets tweets", {
  skip_on_cran()

  n <- 50
  token <- readRDS("twitter_tokens")
  x <- get_timeline(c("@SenDougJones"), n = n, token = token)
  expect_true(is.data.frame(x))
  expect_named(x)
  expect_true("status_id" %in% names(x))
  expect_gt(nrow(x), 100)
  expect_gt(ncol(x), 25)
  xts <- ts_data(x, by = "days")
  expect_true(is.data.frame(xts))
  p <- ts_plot(xts)
  #expect_true(inherits(p, "ggplot"))
  #if (requireNamespace("ggplot2", quietly = TRUE)) {
  g <- readRDS("g.rds")
  p <- ts_plot(g, "hours", trim = 1)
  expect_true(inherits(p, "ggplot"))
  d <- ts_data(g, "hours", trim = 1)
  expect_true(is.data.frame(d))
  #expect_equal(ncol(d), 4)
  unlink("Rplots.pdf")

})

test_that("hormaleR gets tweets", {
  skip_on_cran()

  n <- 50
  token <- readRDS("twitter_tokens")
  x <- get_timeline(c("@SenDougJones"), n = n, token = token)
  expect_true(is.data.frame(x))
  expect_named(x)
  expect_true("status_id" %in% names(x))
  expect_gt(nrow(x), 100)
  expect_gt(ncol(x), 25)
  xts <- ts_data(x, by = "days")
  expect_true(is.data.frame(xts))
  p <- ts_plot(xts)
  #expect_true(inherits(p, "ggplot"))
  #if (requireNamespace("ggplot2", quietly = TRUE)) {
  g <- readRDS("g.rds")
  p <- ts_plot(g, "hours", trim = 1)
  expect_true(inherits(p, "ggplot"))
  d <- ts_data(g, "hours", trim = 1)
  expect_true(is.data.frame(d))
  #expect_equal(ncol(d), 4)
  unlink("Rplots.pdf")
})

test_that("hormaleD gets tweets", {
  skip_on_cran()

  n <- 50
  token <- readRDS("twitter_tokens")
  x <- get_timeline(c("@SenDougJones"), n = n, token = token)
  expect_true(is.data.frame(x))
  expect_named(x)
  expect_true("status_id" %in% names(x))
  expect_gt(nrow(x), 100)
  expect_gt(ncol(x), 25)
  xts <- ts_data(x, by = "days")
  expect_true(is.data.frame(xts))
  p <- ts_plot(xts)
  #expect_true(inherits(p, "ggplot"))
  #if (requireNamespace("ggplot2", quietly = TRUE)) {
  g <- readRDS("g.rds")
  p <- ts_plot(g, "hours", trim = 1)
  expect_true(inherits(p, "ggplot"))
  d <- ts_data(g, "hours", trim = 1)
  expect_true(is.data.frame(d))
  #expect_equal(ncol(d), 4)
  unlink("Rplots.pdf")
})
