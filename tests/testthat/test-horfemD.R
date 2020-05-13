context("horfemD")
library(ScrapeCongress)

context("horfemD")
library(ScrapeCongress)
library(rtweet)
test_that('horfemD', {
  skip_on_cran()
<<<<<<< HEAD
  api_key <- "K9ArCyiCmOaJDTAArBXHDnGo0"
  api_secret <- "1egMquUAtE9Kp5mDVmMomwfFIDrOZQcYoxUBcxUsZzEhtUnegw"
  access_token <- "1114308687261261824-FEK7ybe6LwG4DFB4oofByZvouN6Jiz"
  access_token_secret <- "C0sj3bXJOw3F5fapzMyDx0wkRZ8cR6rg1PNbRdnvnIJX3"
  app_name <- "Congress_Gender"
  key <- rtweet::create_token(
    app = app_name,
    consumer_key = api_key,
    consumer_secret = api_secret,
    access_token = access_token,
    access_secret = access_token_secret
  )
  expect_message(horfemD())
})
test_that('horfemD', {
  skip_on_cran()
  api_key <- "K9ArCyiCmOaJDTAArBXHDnGo0"
  api_secret <- "1egMquUAtE9Kp5mDVmMomwfFIDrOZQcYoxUBcxUsZzEhtUnegw"
  access_token <- "1114308687261261824-FEK7ybe6LwG4DFB4oofByZvouN6Jiz"
  access_token_secret <- "C0sj3bXJOw3F5fapzMyDx0wkRZ8cR6rg1PNbRdnvnIJX3"
  app_name <- "Congress_Gender"
  key <- rtweet::create_token(
    app = app_name,
    consumer_key = api_key,
    consumer_secret = api_secret,
    access_token = access_token,
    access_secret = access_token_secret
  )
  expect_warning(horfemD())
=======
  token <- rtweet::get_token()
  f <- senmaleD()
  expect_message(f, "Check your Data Folder. Function ran successfully")
>>>>>>> parent of 0badd50... Final Changes before CRAN
})
