# Title: test of gen_count

# Notes
  #* Description
    #** Testing Script for the gen_count() function
  #* Updated
    #** 2023-12-01
    #** dcr

# Setup
  #* Load handy functions for testing
library(testthat)
devtools::load_all()

# Text for testing
text <- 'This person was a heroine due to their fighting during the war.'

# Testing
  #* Check that it returns dataframe
test_that("Check that it returns dataframe", {
  expect_s3_class(gen_count(text), "data.frame")
})
  #* Check that it returns 3 rows
test_that("Check that it returns three rows", {
  expect_true(nrow(gen_count(text)) == 3)
})
  #* Check that it returns 4 columns
test_that("Check that it returns four columns", {
  expect_length(gen_count(text), 4)
})