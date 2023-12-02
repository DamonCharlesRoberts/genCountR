#' @title
#' Data from Gendered Language Dictionary Developed by Roberts and Utych (2019)
#'
#' @description
#' Each word in the dataset contains a rating by human coders. See details of dataset in the original paper.
#'
#' @format A tibble with 701 rows and 15 columns:
#' \describe{
#'   \item{Word}{The word to match}
#'   \item{POS}{Part Of Speech}
#'   \item{mean.a}{Mean score provided by all participants}
#'   \item{std.dev.a}{Standard deviation of score provided by all participants}
#' }
#' @source \url{https://journals.sagepub.com/doi/10.1177/1065912919874883}
"dict"