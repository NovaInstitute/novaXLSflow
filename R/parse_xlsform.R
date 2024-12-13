#' Parse an XLSForm file into a flowchart structure
#'
#' This function parses an XLSForm (XLSX file) and extracts a hierarchical structure of questions,
#' groups, and repeats. It also identifies logical relationships from `relevant`, `constraint`, and
#' `calculation` fields, producing a node-edge representation of the form logic.
#'
#' The returned `nodes` data frame includes columns:
#' \itemize{
#'   \item \code{id}: The variable name of the question or group.
#'   \item \code{label}: The label text of the question or group (if available).
#'   \item \code{qtype}: The question type (e.g., "integer", "select_one", "begin_group").
#' }
#'
#' The returned `edges` data frame includes columns:
#' \itemize{
#'   \item \code{from}: The ID of a node that influences the logic of another node.
#'   \item \code{to}: The ID of the node being influenced.
#'   \item \code{type}: The type of relationship ("hierarchy", "relevant", "constraint", "calculation").
#'   \item \code{condition}: The expression (e.g., relevance, constraint, calculation) associated with the edge.
#' }
#'
#' @param xls_path A character string specifying the path to the XLSForm (XLSX) file.
#' @return A list with two data frames: `nodes` and `edges`.
#' @export
#' @examples
#' \dontrun{
#'   flowchart_data <- parse_xlsform("path/to/your_form.xlsx")
#'   str(flowchart_data)
#' }
parse_xlsform <- function(filepath) {
        library(dplyr)
        library(openxlsx)
        library(stringr)
        library(tibble)

        survey_df <- read.xlsx(filepath, sheet = "survey")

        # Predefine nodes and edges data frames with required columns
        nodes <- tibble(
                id = character(),
                label = character(),
                qtype = character(),
                stringsAsFactors = logical()
        )

        edges <- tibble(
                from = character(),
                to = character(),
                type = character(),
                condition = character(),
                stringsAsFactors = logical()
        )

        # Add the 'root' node as tests expect it
        root_node <- tibble(
                id = "root",
                label = "root",
                qtype = "root",
                stringsAsFactors = FALSE
        )
        nodes <- bind_rows(nodes, root_node)

        # Initialize stack with 'root'
        stack <- list("root")

        # Helper function to add an edge row easily
        add_edge <- function(from, to, type, condition = NA) {
                edge_entry <- tibble(
                        from = from,
                        to = to,
                        type = type,
                        condition = condition,
                        stringsAsFactors = FALSE
                )
                edge_entry
        }

        for (i in seq_len(nrow(survey_df))) {
                row <- survey_df[i,]
                type <- row$type
                name <- row$name
                lbl <- row$label
                if (is.na(lbl)) lbl <- name
                relevant <- row$relevant
                constraint <- row$constraint
                calculation <- row$calculation

                # qtype is exactly the XLSForm type
                qtype_clean <- type

                # If calculate, label is calculation if available
                final_label <- if (isTRUE(qtype_clean == "calculate" && !is.na(calculation) && nzchar(calculation))) {
                        calculation
                } else if (nzchar(lbl)) {
                        lbl
                } else {
                        name
                }

                if (grepl("^begin_group", type) || grepl("^begin_repeat", type)) {
                        # Add group/repeat node
                        new_node <- tibble(
                                id = name,
                                label = final_label,
                                qtype = qtype_clean,
                                stringsAsFactors = FALSE
                        )
                        nodes <- bind_rows(nodes, new_node)

                        # Always add hierarchy edge from current stack top
                        parent_id <- stack[[length(stack)]]
                        edges <- bind_rows(edges, add_edge(parent_id, name, "hierarchy", NA))

                        # Push current node onto stack
                        stack <- c(stack, list(name))

                } else if (grepl("^end_group", type) || grepl("^end_repeat", type)) {
                        # End of group/repeat
                        if (length(stack) == 1) {
                                stop("Mismatched end_group/end_repeat without corresponding begin.")
                        }
                        stack <- stack[-length(stack)]

                } else {
                        # It's a question or calculation
                        new_node <- tibble(
                                id = name,
                                label = final_label,
                                qtype = qtype_clean,
                                stringsAsFactors = FALSE
                        )
                        nodes <- bind_rows(nodes, new_node)

                        # If stack not empty, add hierarchy edge from top of stack
                        parent_id <- stack[[length(stack)]]
                        edges <- bind_rows(edges, add_edge(parent_id, name, "hierarchy", NA))

                        # Constraint edges
                        if (isTRUE(!is.na(constraint) && nzchar(constraint))) {
                                vars <- str_match_all(constraint, "\\$\\{([^}]+)\\}")[[1]][,2]
                                vars <- unique(vars)
                                for (v in vars) {
                                        edges <- bind_rows(edges, add_edge(v, name, "constraint", constraint))
                                }
                        }

                        # Calculation edges
                        if (isTRUE(!is.na(calculation) && nzchar(calculation))) {
                                vars <- str_match_all(calculation, "\\$\\{([^}]+)\\}")[[1]][,2]
                                vars <- unique(vars)
                                for (v in vars) {
                                        edges <- bind_rows(edges, add_edge(v, name, "calculation", calculation))
                                }
                        }

                        # Relevance edges
                        if (isTRUE(!is.na(relevant) && nzchar(relevant))) {
                                vars <- str_match_all(relevant, "\\$\\{([^}]+)\\}")[[1]][,2]
                                vars <- unique(vars)
                                for (v in vars) {
                                        edges <- bind_rows(edges, add_edge(v, name, "relevant", relevant))
                                }
                        }
                }
        }

        if (length(stack) > 1) {
                stop("Some groups or repeats were not closed properly.")
        }

        list(nodes = nodes, edges = edges)
}
