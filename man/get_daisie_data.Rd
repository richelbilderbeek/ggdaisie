% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_daisie_data.R
\name{get_daisie_data}
\alias{get_daisie_data}
\title{Read a data frame from the \code{data} folder}
\usage{
get_daisie_data(filename)
}
\arguments{
\item{filename}{the file's name, without the path}
}
\value{
the content of the file as a data frame,
  if and only if the file is present. Will stop otherwise.
}
\description{
Read a data frame from the \code{data} folder
}
\examples{
  testit::assert(
    is.data.frame(
      get_daisie_data("Galapagos_datatable.RData")
    )
  )
}
\author{
Richel J.C. Bilderbeek
}
