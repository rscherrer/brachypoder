#' Read simulation data
#'
#' Combine data from one simulation, saved as binary files, into a single
#' tibble.
#'
#' @param root Path to the simulation folder
#' @param variables Vector of names of variable to read (e.g. \code{c("time", "popsize")})
#' @param ncols Vector or list indicating how to split or duplicate each variable
#' when constructing the tibble. There must be one element per variable.
#' See \code{?sim2tbl} for details.
#'
#' @details Each variable is read as a vector and reshaped into a tibble by being
#' passed, with its corresponding \code{ncols}, to \code{sim2tbl}.
#' The resulting tibbles for all variables are then bound together by column.
#'
#' @return A tibble containing the simulation data
#'
#' @note Do not provide the extension of the data files in \code{variables} (".dat").
#'
#' @seealso \code{sim2tbl}
#'
#' @examples
#'
#' root <- system.file("extdata", "sim-example", package = "brachypoder")
#' read_data(root, "time")
#' read_data(root, c("time", "popsize"))
#'
#' @export

# Function to read simulation data
read_data <- function(root, variables, ncols = rep(1, length(variables))) {

  if (length(variables) != length(ncols)) stop("ncols and variables must have the same length")

  # Paths to the data files
  data_file_names <- paste0(root, "/", variables, ".dat")

  # Read in the variables as vectors
  data <- purrr::map(data_file_names, read_binary)

  # Transform the data vectors into tibbles
  data <- suppressMessages(purrr::map2_dfc(data, ncols, sim2tbl))

  # Update the number of columns allocated to each variable
  ncols[purrr::map_lgl(ncols, ~ length(.x) > 1)] <- 1
  ncols <- purrr::reduce(ncols, c)

  # Update column names
  colnames(data) <- index_duplicate(variables, ncols)

  return(data)

}
