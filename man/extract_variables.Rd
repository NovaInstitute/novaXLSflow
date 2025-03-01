% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Internal.R
\name{extract_variables}
\alias{extract_variables}
\title{Extract variable names from an XLSForm expression}
\usage{
extract_variables(expr)
}
\arguments{
\item{expr}{A character string containing an XLSForm expression.}
}
\value{
A character vector of unique variable names referenced in `expr`.
}
\description{
This function extracts all variable references of the form `${varname}` from a given expression.
}
\keyword{internal}
