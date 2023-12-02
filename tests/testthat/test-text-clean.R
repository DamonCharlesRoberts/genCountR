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

# Tests
  #* Check that it returns list object
test_that("check that it returns list object", {
    expect_type(text_clean(text), "character")
})
  #* Check that it is of length 6
test_that("Check that it is of length 6", {
    expect_length(text_clean(text), 6)
})
