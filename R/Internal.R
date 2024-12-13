#' Extract variable names from an XLSForm expression
#'
#' This function extracts all variable references of the form `${varname}` from a given expression.
#'
#' @param expr A character string containing an XLSForm expression.
#' @return A character vector of unique variable names referenced in `expr`.
#' @keywords internal
extract_variables <- function(expr) {
        if (is.na(expr) || expr == "") return(character(0))
        matches <- stringr::str_match_all(expr, "\\$\\{([^}]+)\\}")
        unique(matches[[1]][,2])
}
#' Create a single XLSForm node as a tibble
#'
#' This internal helper function creates a single node of the XLSForm hierarchy.
#' Each node is represented as a one-row tibble with columns:
#' \itemize{
#'   \item \code{id}: The node's identifier (question name, group name, etc.).
#'   \item \code{label}: The display label of the node.
#'   \item \code{qtype}: The XLSForm type (e.g., \code{integer}, \code{begin_group}, etc.).
#'   \item \code{relevant}: The relevance expression if any.
#'   \item \code{constraint}: The constraint expression if any.
#'   \item \code{calculation}: The calculation expression if any.
#'   \item \code{children}: A list-column containing a tibble of child nodes (initially empty).
#' }
#'
#' @param id Character. The ID/name of the node.
#' @param label Character. The label text.
#' @param qtype Character. The XLSForm type.
#' @param relevant Character. The relevance expression, or \code{NA}.
#' @param constraint Character. The constraint expression, or \code{NA}.
#' @param calculation Character. The calculation expression, or \code{NA}.
#' @return A one-row tibble representing a single node.
#' @keywords internal
create_node <- function(id, label, qtype, relevant, constraint, calculation) {
        node <- new.env(parent = emptyenv())
        node$id <- id

        # Clean qtype by trimming whitespace and converting to lowercase
        qtype_clean <- tolower(trimws(qtype))

        # Debugging: Print qtype_clean to verify its value
        cli::cli_alert_info("qtype_clean for node '{id}': '{qtype_clean}'")

        # Assign label based on type and availability
        if (is.na(label) || label == "") {
                node$label <- id
                cli::cli_alert_info("Node '{id}' has no label. Label set to id: '{id}'")
        } else {
                node$label <- label
                cli::cli_alert_info("Node '{id}' has label: '{label}'")
        }
        if (qtype_clean == "calculate") {
                node$label <- calculation
                cli::cli_alert_info("Node '{id}' is a calculate type. Label set to calculation: '{calculation}'")
        }

        node$qtype <- qtype
        node$relevant <- relevant
        node$constraint <- constraint
        node$calculation <- calculation
        node$children <- list() # Initialize as an empty list
        node
}

# Helper to add a child node to a parent node
#' Add a child node to a parent node tibble
#'
#' This internal helper function appends a single-node tibble to the \code{children}
#' column of a parent node tibble. Both \code{parent} and \code{child} must be
#' one-row tibbles created by \code{\link{create_node}}.
#'
#' @param parent A one-row tibble representing the parent node.
#' @param child A one-row tibble representing the child node.
#' @return The parent tibble with the child node added to the \code{children}.
#' @keywords internal
add_child <- function(parent, child) {
        parent$children <- c(parent$children, list(child))
}

