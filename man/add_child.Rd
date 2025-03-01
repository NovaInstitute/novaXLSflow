% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Internal.R
\name{add_child}
\alias{add_child}
\title{Add a child node to a parent node tibble}
\usage{
add_child(parent, child)
}
\arguments{
\item{parent}{A one-row tibble representing the parent node.}

\item{child}{A one-row tibble representing the child node.}
}
\value{
The parent tibble with the child node added to the \code{children}.
}
\description{
This internal helper function appends a single-node tibble to the \code{children}
column of a parent node tibble. Both \code{parent} and \code{child} must be
one-row tibbles created by \code{\link{create_node}}.
}
\keyword{internal}
