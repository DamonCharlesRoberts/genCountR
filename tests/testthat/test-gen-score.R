# Title: test of gen_count

# Notes
  #* Description
    #** Testing Script for the gen_count() function
  #* Updated
    #** 2023-12-02
    #** dcr

# Setup
  #* Load handy functions for testing
library(testthat)
devtools::load_all()

# Text for testing
text <- 'Hero. hero Heroine. heroine, Prison. Prisom.'

# Testing
  #* Check that it returns a list object
test_that("check that it returns a list object", {
    expect_type(gen_score(text), "list")
})
  #* Check that it returns 3 elements in the list
test_that("check that it returns three elements", {
    expect_length(gen_score(text), 3)
})
  #* Check that the first element returns a numeric
test_that("check that the first element is numeric", {
    expect_type(gen_score(text)[[1]], "double")
})
  #* Check that the second element returns a numeric
test_that("check that the second element is numeric", {
    expect_type(gen_score(text)[[2]], "double")
})
  #* Check that the third element returns a data.frame
test_that("check that the third element is data.frame", {
    expect_s3_class(gen_score(text)[[3]], "data.frame")
})
