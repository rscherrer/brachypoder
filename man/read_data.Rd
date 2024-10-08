% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_data.R
\name{read_data}
\alias{read_data}
\title{Read simulation data}
\usage{
read_data(root, variables, ncols = rep(1, length(variables)))
}
\arguments{
\item{root}{Path to the simulation folder}

\item{variables}{Vector of names of variable to read (e.g. \code{c("time", "popsize")})}

\item{ncols}{Vector or list indicating how to split or duplicate each variable
when constructing the tibble. There must be one element per variable.
See \code{?sim2tbl} for details.}
}
\value{
A tibble containing the simulation data
}
\description{
Combine data from one simulation, saved as binary files, into a single
tibble.
}
\details{
Each variable is read as a vector and reshaped into a tibble by being
passed, with its corresponding \code{ncols}, to \code{sim2tbl}.
The resulting tibbles for all variables are then bound together by column.
}
\note{
Do not provide the extension of the data files in \code{variables} (".dat").
}
\examples{

root <- system.file("extdata", "sim-example", package = "brachypoder")
read_data(root, "time")
read_data(root, c("time", "popsize"))

}
\seealso{
\code{sim2tbl}
}
