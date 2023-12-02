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
#' word_item (vector or array): A vector or array of words from the text wanting to be matched to dictionary.
#'
#' @return
#' data.frame object of with count of masculine, feminine, and masculine words.
#'
#'
word_count <- function(
  word_item
) {
  # Filter the words that are in the document
  unique_words <- unique(word_item)
  matched <- unique_words[unique_words %in% genCountR::dict$Word]

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