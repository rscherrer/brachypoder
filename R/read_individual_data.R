#' Read individual data
#'
#' This function is a wrapper around \code{read_data} tailored at reading the
#' output file "individuals.dat" into a tibble with one row per individual and
#' individual attributes (e.g. deme, patch and traits) in columns, properly
#' renamed.
#'
#' @param root Path to the simulation folder
#'
#' @return A tibble
#'
#' @seealso \code{read_data}
#'
#' @examples
#'
#' root <- system.file("extdata", "sim-example", package = "brachypoder")
#' read_individual_data(root)
#'
#'@export

read_individual_data <- function(root) {

  # Read the data and split them into the corresponding columns
  data <- read_data(root, variables = "individuals", ncols = 5)

  # Read time points
  timepoints <- read_binary(paste0(root, "/time.dat"))

  # Read population sizes at each time point
  popsizes <- read_binary(paste0(root, "/popsize.dat"))

  # Add a time column at the beginning
  data$time <- purrr::reduce(purrr::map2(timepoints, popsizes, rep), c)
  data <- data[, c(ncol(data), seq(ncol(data) - 1))]

  # Rename the columns
  names(data) <- c("time", "deme", "patch", "x", "y", "z")

  return(data)

}
