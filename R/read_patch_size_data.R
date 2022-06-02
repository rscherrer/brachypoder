#' Read patch size data
#'
#' This function is a wrapper around \code{read_data} tailored at reading the
#' output file "patchsizes.dat" into a tibble with one row per deme and per patch.
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
#' read_patch_size_data(root)
#'
#'@export

read_patch_size_data <- function(root) {

  pars <- read_parameters(root)
  ndemes <- length(pars$pgood) - 1

  # Read the data and split them into the corresponding columns
  data <- read_data(root, variables = "patchsizes")

  data$patch <- rep(0:1, nrow(data) / 2)
  ntimepoints <- nrow(data) / (ndemes * 2)
  data$deme <- rep(rep(seq(ndemes), each = 2), ntimepoints)

  # Read time points
  timepoints <- read_binary(paste0(root, "/time.dat"))
  data$time <- rep(timepoints, each = ndemes * 2)

  # Reorder columns
  data <- data[, c(4, 3, 2, 1)]

  # Rename the columns
  names(data) <- c("time", "deme", "patch", "n")

  data$n <- as.integer(data$n)

  return(data)

}
