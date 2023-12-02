# Title: Load Dictionary

# Notes
  #* Description
    #** Script to create dictionary dataset
  #* Updated
    #** 2023-12-01
    #** dcr

# Load dataset
dict <- utils::read.csv(
  file = './assets/dict.csv'
)

# Store the dataset
usethis::use_data(dict, overwrite = TRUE)