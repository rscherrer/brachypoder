#' Turn a data vector into a tibble
#'
#' Reshapes a simulation data vector into a matrix and converts it into a tibble.
#'
#' @param x Vector of data values
#' @param ncol If integer, number of columns by which to split the vector.
#' Use a negative value to duplicate the vector into a longer one-column tibble instead of
#' splitting it. If integer vector, then each element in the \code{x} will be
#' duplicated a number of times defined by its corresponding (positive) entry in
#' \code{ncol}. So in this case, \code{ncol} and \code{x} must have the same length.
#'
#' @return A tibble containing the simulation data
#'
#' @seealso \code{read_data}
#'
#' @examples
#'
#' sim2tbl(1:3)
#' sim2tbl(1:3, ncol = 3)
#' sim2tbl(1:3, ncol = -2)
#' sim2tbl(1:3, ncol = 1:3)
#'
#' @export

# Function to turn a vector of values into a tibble
sim2tbl <- function(x, ncol = 1) {

  # Duplicate different numbers of times if needed...
  if (length(ncol) > 1) {

    if (length(ncol) != length(x)) stop("x and ncol must have the same length if ncol is a vector")
    return(sim2tbl_mdupl(x, ncol))

  }

  if (ncol == 0) stop("Invalid number of columns")
  if (ncol == 1) return(tibble::tibble(V1 = x))

  # Duplicate if needed...
  if (ncol < 0) x <- sim2tbl_dupl(x, abs(ncol))

  # Otherwise split
  if (ncol > 1) x <- sim2tbl_split(x, ncol)

  return(x)

}

# Function to split a vector into a tibble with multiple columns
sim2tbl_split <- function(x, ncol) {

  # Split the vector into multiple columns
  x <- matrix(x, ncol = ncol, nrow = length(x) / ncol, byrow = TRUE)
  colnames(x) <- paste0("V", seq(ncol(x)))

  # Return a tibble
  return(tibble::as_tibble(x))

}

# Function to duplicate elements and put them in a one-column tibble
sim2tbl_dupl <- function(x, ncol) {

  # Duplicate the data
  x <- rep(x, each = ncol)

  return(tibble::tibble(V1 = x))

}

# Same but with different elements duplicated different numbers of times
sim2tbl_mdupl <- function(x, ncol) {

  # Duplicate the data
  x <- purrr::reduce(purrr::map2(x, ncol, ~ rep(.x, .y)), c)

  return(tibble::tibble(V1 = x))

}
