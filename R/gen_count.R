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
#' text (string): A string object.
#'
#' @return
#' data.frame with each word from the dictionary matched with the text and its number of occurances.
#'
#' @examples
#' text <- 'This person was a heroine due to their fighting during the war.'
#' result_df <- genCountR::gen_count(text)
#'
#' @export
gen_count <- function(
  text
) {
  # Clean the text in the document
  word_list <- text_clean(text)

  # Execute the function to count words
  df <- word_count(word_list)

  # Merge with dictionary for their score
  df <- merge(df, genCountR::dict, how="inner", by.x = "word", by.y = "Word")

  # Create column that assigns label
  result_df <- data.frame(
    word = df$word
    , count = df$count
    , score = df$mean.a
  )
  result_df["classified"] <- ifelse(
    result_df$score < 2.5, "Feminine" # if the score is below 2.5, feminine
    , ifelse(
      result_df$score >= 5.5, "Masculine" # if the score is above or equal to 5.5, masculine
      , "Neutral" # all other scores should be labelled neutral
    )
  )
  # Return the dataframe
  return(result_df)
}