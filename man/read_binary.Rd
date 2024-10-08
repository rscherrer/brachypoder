% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_binary.R
\name{read_binary}
\alias{read_binary}
\title{Read a binary file}
\usage{
read_binary(filename, nbytes = 8)
}
\arguments{
\item{filename}{Path to the file}

\item{nbytes}{The number of bytes constituting one value (e.g. 8 bytes
for a double)}
}
\value{
A numeric vector
}
\description{
Read a binary file
}
\examples{

root <- system.file("extdata", "sim-example", package = "brachypoder")
read_binary(paste0(root, "/time.dat"))

}
