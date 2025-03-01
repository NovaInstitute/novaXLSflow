% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/build_hierarchy.R
\name{build_hierarchy}
\alias{build_hierarchy}
\title{Build a hierarchical representation of an XLSForm using nested tibbles}
\usage{
build_hierarchy(survey_df)
}
\arguments{
\item{survey_df}{A data frame representing the `survey` sheet of an XLSForm.
It is assumed that `survey_df` has columns `type`, `name`, `label`, `relevant`,
`constraint`, and `calculation` (with `NA` where not applicable).}
}
\value{
A tibble representing the top-level nodes. Each row corresponds to a top-level node
  (usually groups or questions). Each node contains a `children` column that may contain further
  nested nodes. If the form has a flat structure (no groups), it returns a tibble of questions.
}
\description{
This function processes the `survey` data frame, identifying groups, repeats, and questions,
and constructs a nested tibble structure representing the hierarchy of the form.
}
\details{
Each node is a one-row tibble created by \code{\link{create_node}}, and nodes are nested
via the `children` list-column. Groups and repeats contain their subordinate questions and
other nested groups/repeats in `children`.
}
\examples{
\dontrun{
survey_df <- data.frame(
  type = c("begin_group", "integer", "text", "end_group"),
  name = c("personal_info", "age", "notes", "personal_info_end"),
  label = c("Personal Info", "Your age?", "Any notes?", NA),
  relevant = c(NA, NA, NA, NA),
  constraint = c(NA, NA, NA, NA),
  calculation = c(NA, NA, NA, NA),
  stringsAsFactors = FALSE
)
hierarchy <- build_hierarchy(survey_df)
}
}
\keyword{internal}
