#' @title gen_score
#' 
#' @description
#' Calculates the score of the supplied text string based on the Gendered Language Dictionary created by Roberts and Utych (2019).
#' 
#' @details
#' Takes the matched words and their occurrence in the supplied text, finds the score for those matched words in the dictionary, sums those scores up and then divides it by the total number of words in the dictionary.
#' 
#' @param 
#' text (string): A string object
#' 
#' @return 
#' list object with avg_score of the supplied text string, total_score of supplied text string, and data.frame of matches
#' 
#' @examples
#' text <- 'Hero. hero Heroine. heroine, Prison. Prisom.'
#' result <- genCountR::gen_score(text)
#' result$avg_score
#' result$total_score
#' result$df
#' 
#' @export
gen_score <- function(
    text
) {
    # Create empty list object
    result <- list(
        "avg_score" = numeric(0)
        , "total_score" = numeric(0)
        , "df" = data.frame(
            "word" = character(0)
            , "count" = integer(0)
            , "score" = numeric(0)
            , "total_score" = numeric(0)
        )
    )
    # Clean the text in the document
    word_list <- text_clean(text)

    # Execute the function to count words
    df <- word_count(word_list)

    # Merge the score
    df <- base::merge(df, genCountR::dict, how="inner", by.x = "word", by.y = "Word")

    # Create a data.frame for the score
    result[["df"]] <- data.frame(
      "word" = df["word"]
      , "count" = df["count"]
      , "score" = df["mean.a"]
    )
    
    # Calculate total row-wise score
    result[["df"]]["total_score"] <- (result[["df"]]["count"] * result[["df"]]["mean.a"])

    # Get the total score for the document
    result[["total_score"]] <- sum(result[["df"]]["total_score"])

    # Get the average score for the document
    result[["avg_score"]] <- (result[["total_score"]]/length(word_list))
    
    # Return result
    return(result)
}