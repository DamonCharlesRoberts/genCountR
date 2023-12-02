#' @title word_count
#'
#' @description
#' Count number of times a particular word from the dictionary shows up in a document.
#'
#' @details
#' Takes the number of words that are loosely categorized as Masculine, Feminine, or Neutral based on Roberts and Utych's (2019) definition.
#' Feminine words had a score below 2.5, Neutral words had a score higher than 2.5 and lower than 5.5, Masculine words had a score higher than 5.5.
#'
#' @param
#' word_list (vector or array): A vector or array of words from the text wanting to be matched to dictionary.
#'
#' @return
#' data.frame object of with count of masculine, feminine, and masculine words.
#'
#' @examples
#' word_list <- c("heroine", "war", "fighting")
#' result_df <- word_count(word_list)
#'
word_count <- function(
  word_item
) {
  # Filter the words that are in the document
  unique_words <- unique(word_item)
  matched <- unique_words[unique_words %in% dict$Word]

  # Count number of times the word shows up
  count <- sapply(matched, function(w) sum(word_item == w))

  # Create a dataframe for this current row
  row_df <- base::data.frame(
    word = matched
    , count = count
  )

  # Return the row
  return(row_df)
}

#' @title gen_count
#'
#' @description
#' Counts the number of masculine and feminine words in the document
#'
#' @details
#' Takes the number of words that are loosely categorized as Masculine, Feminine, or Neutral based on Roberts and Utych's (2019) definition.
#' Feminine words had a score below 2.5, Neutral words had a score higher than 2.5 and lower than 5.5, Masculine words had a score higher than 5.5.
#'
#' @param
#' text (string): A string object of text.
#'
#' @return
#' data.frame with each word from the dictionary matched with the text and its number of occurances.
#'
#' @examples
#' text <- 'This person was a heroine due to their fighting during the war.'
#' result_df <- gen_count(text)
#'
gen_count <- function(
  text
) {
  # Clean the text in the document
  text_clean <- base::tolower(text)
  text_clean <- base::gsub("[[:punct:]]", " ", text_clean)
  word_list <- base::strsplit(text_clean, "\\s+")[[1]]

  # Execute the function
  result_df <- word_count(word_list)

  # Return the dataframe
  return(result_df)
}