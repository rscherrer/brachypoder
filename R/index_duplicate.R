#' Duplicate strings with indices
#'
#' Repeat strings in a vector and append an index to each repeat.
#'
#' @param x A vector of strings
#' @param n A vector of numbers of times each string is to be repeated
#'
#' @return A vector of strings

# Function to duplicate string from a vector with indices
index_duplicate <- function(x, n) {

  # Duplicate, index and concatenate
  x <- purrr::map2(x, n, ~ { if (.y > 1) paste0(.x, seq(.y)) else .x })
  x <- purrr::reduce(x, c)

  return(x)

}
