#' Build a hierarchical representation of an XLSForm using nested tibbles
#'
#' This function processes the `survey` data frame, identifying groups, repeats, and questions,
#' and constructs a nested tibble structure representing the hierarchy of the form.
#'
#' Each node is a one-row tibble created by \code{\link{create_node}}, and nodes are nested
#' via the `children` list-column. Groups and repeats contain their subordinate questions and
#' other nested groups/repeats in `children`.
#'
#' @param survey_df A data frame representing the `survey` sheet of an XLSForm.
#'   It is assumed that `survey_df` has columns `type`, `name`, `label`, `relevant`,
#'   `constraint`, and `calculation` (with `NA` where not applicable).
#'
#' @return A tibble representing the top-level nodes. Each row corresponds to a top-level node
#'   (usually groups or questions). Each node contains a `children` column that may contain further
#'   nested nodes. If the form has a flat structure (no groups), it returns a tibble of questions.
#'
#' @keywords internal
#' @examples
#' \dontrun{
#' survey_df <- data.frame(
#'         type = c("begin_group", "integer", "text", "end_group"),
#'         name = c("personal_info", "age", "notes", "personal_info_end"),
#'         label = c("Personal Info", "Your age?", "Any notes?", NA),
#'         relevant = c(NA, NA, NA, NA),
#'         constraint = c(NA, NA, NA, NA),
#'         calculation = c(NA, NA, NA, NA),
#'         stringsAsFactors = FALSE
#' )
#' hierarchy <- build_hierarchy(survey_df)
#' }
build_hierarchy <- function(survey_df) {
        cli::cli_h1("Building Hierarchy")

        # Initialize the root node
        root <- create_node("root", "root", "root", NA, NA, NA)

        # Initialize the stack with the root node
        stack <- list(root)

        cli::cli_text("Initialized root node: {root$id}")

        for (i in seq_len(nrow(survey_df))) {
                row <- survey_df[i, ]
                tp <- row$type
                nm <- row$name
                lb <- ifelse(is.na(row$label), nm, row$label)
                rel <- row$relevant
                con <- row$constraint
                cal <- row$calculation

                cli::cli_h3("Processing row {i}: Type='{tp}', Name='{nm}'")

                if (grepl("^begin_group", tp) || grepl("^begin_repeat", tp)) {
                        # Begin a new group or repeat
                        group_node <- create_node(nm, lb, tp, rel, con, cal)

                        # Get the current parent node (top of the stack)
                        current_parent <- stack[[length(stack)]]

                        # Add the new group/repeat as a child of the current parent
                        add_child(current_parent, group_node)

                        # Push the new group node onto the stack
                        stack <- c(stack, list(group_node))

                        cli::cli_alert_info("Added {tp} node '{nm}' as a child of '{current_parent$id}'")
                        cli::cli_text("Pushed '{nm}' onto the stack")
                } else if (grepl("^end_group", tp) || grepl("^end_repeat", tp)) {
                        # End the current group or repeat
                        if (length(stack) == 1) {
                                cli::cli_alert_danger("Mismatched {tp} without corresponding begin")
                                stop("Mismatched end_group/end_repeat without a corresponding begin.")
                        }

                        # Pop the stack to return to the previous parent
                        popped_node <- stack[[length(stack)]]
                        stack <- stack[-length(stack)]

                        cli::cli_alert_info("Ended {tp}. Popped '{popped_node$id}' from the stack. Current top: '{stack[[length(stack)]]$id}'")
                } else {
                        # It's a question or calculation node
                        q_node <- create_node(nm, lb, tp, rel, con, cal)

                        # Get the current parent node (top of the stack)
                        current_parent <- stack[[length(stack)]]

                        # Add the question/calculation as a child of the current parent
                        add_child(current_parent, q_node)

                        cli::cli_alert_info("Added node '{nm}' as a child of '{current_parent$id}'")
                }

                # Add a visual separator between processing of different rows
                cli::cli_rule()
        }

        if (length(stack) > 1) {
                cli::cli_alert_danger("Some groups or repeats were not closed properly.")
                stop("Some groups or repeats were not closed properly.")
        }

        cli::cli_alert_success("Hierarchy building completed successfully.")

        cli::cli_h2("Final Hierarchy Structure")
        # Function to print hierarchy
        print_hierarchy <- function(node, indent = 0) {
                prefix <- paste(rep("  ", indent), collapse = "")
                cli::cli_text("{prefix}- {node$id} ({node$qtype}): {node$label}")
                for (child in node$children) {
                        print_hierarchy(child, indent + 1)
                }
        }

        print_hierarchy(root)

        root
}
