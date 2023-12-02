#' @title text_clean
#' 
#' @description
#' Cleans the supplied text string and converts it into a list of individual words.
#' 
#' @details
#' Takes the string, converts all the characters to lower case, removes punctuation, and splits the string into individual words.
#' 
#' @param
#' text (string): A string object.
#' 
#' @return
#' list of each word in all lower case and without punctuation.
#'
text_clean <- function(
  text
){
  # Convert all characters to lower case
  text_clean <- base::tolower(text)

  # Remove punctuation
  text_clean <- base::gsub("[[:punct:]]", " ", text_clean)

  # Split into list
  word_list <- base::strsplit(text_clean, "\\s+")[[1]]

  # Return word list
  return(word_list)
}