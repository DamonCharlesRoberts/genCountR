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
word_list <- c("hero", "heroine", "prison")

# Tests
  #* Correct type returned
test_that("Check to make sure that it returns a data.frame",
{
  expect_s3_class(word_count(word_list), "data.frame")
})
  #* Check contents of it
test_that("Check that there are three rows", {
    expect_true(nrow(word_count(word_list)) == 3)
})
