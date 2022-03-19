#' Read simulation parameters
#'
#' Read the parameter file in a simulation folder
#'
#' @param root Path to the simulation folder
#'
#' @details The function looks for the file "paramlog.txt" in the simulation folder
#'
#' @return A named list of parameter values
#'
#' @examples
#'
#' root <- system.file("extdata", "sim-example", package = "brachypoder")
#' read_parameters(root)
#'
#' @export

read_parameters <- function(root) {

  # Set the parameter file name
  param_file_name <- paste0(root, "/paramlog.txt")

  # Read the lines in the parameter file
  pars <- readLines(param_file_name)

  # Split the lines into parameter name and values
  pars <- stringr::str_split(pars, " ")

  # Extract parameter names
  par_names <- purrr::map_chr(pars, dplyr::first)

  # Extract parameter values as numeric (remove the occasional empty string)
  pars <- purrr::map(pars, ~ as.numeric(.x[.x != ""][-1]))

  # Name the list
  names(pars) <- par_names

  return(pars)

}
