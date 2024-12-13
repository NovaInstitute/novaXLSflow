#' Flatten Hierarchy into Nodes and Edges
#'
#' @param root_node The root node environment to start flattening from.
#' @return A list containing `nodes` and `edges` data frames.
flatten_hierarchy <- function(root_node) {
        cli::cli_h1("Flattening Hierarchy")

        node_list <- list()
        edge_list <- list()

        # Start recursion from each child of root
        for (child in root_node$children) {
                result <- flatten_hierarchy_recursive(child, parent_id = "root", node_list, edge_list)
                node_list <- c(node_list, result$nodes)
                edge_list <- c(edge_list, result$edges)
        }

        # Combine node_list and edge_list
        node_df <- if (length(node_list) > 0) {
                dplyr::bind_rows(node_list)
        } else {
                tibble::tibble(id = character(), label = character(), qtype = character(), stringsAsFactors = FALSE)
        }

        edge_df <- if (length(edge_list) > 0) {
                dplyr::bind_rows(edge_list)
        } else {
                tibble::tibble(from = character(), to = character(), type = character(), condition = character(), stringsAsFactors = FALSE)
        }

        cli::cli_alert_success("Completed flattening. Total nodes: {nrow(node_df)}, Total edges: {nrow(edge_df)}")

        list(nodes = node_df, edges = edge_df)
}

# Internal function to recursively flatten hierarchy
flatten_hierarchy_recursive <- function(node, parent_id = NULL, node_list = list(), edge_list = list()) {
        node_id <- node$id
        node_label <- node$label
        node_qtype <- node$qtype
        node_relevant <- node$relevant
        node_constraint <- node$constraint
        node_calculation <- node$calculation
        node_children <- node$children

        cli::cli_text("Processing node '{node_id}': {node_label} (Type: {node_qtype})")

        # Add node to node_list
        node_entry <- tibble::tibble(
                id = node_id,
                label = node_label,
                qtype = node_qtype,
                stringsAsFactors = FALSE
        )
        node_list[[length(node_list) + 1]] <- node_entry

        # Hierarchy edges
        if (!is.null(parent_id)) {
                cli::cli_alert_info("Adding hierarchy edge from '{parent_id}' to '{node_id}'")
                edge_entry <- tibble::tibble(
                        from = parent_id,
                        to = node_id,
                        type = "hierarchy",
                        condition = NA_character_,
                        stringsAsFactors = FALSE
                )
                edge_list[[length(edge_list) + 1]] <- edge_entry
        }

        # Relevance edges
        if (!is.na(node_relevant) && node_relevant != "") {
                vars <- extract_variables(node_relevant)
                for (v in vars) {
                        cli::cli_alert_info("Adding relevance edge from '{v}' to '{node_id}' with condition '{node_relevant}'")
                        edge_entry <- tibble::tibble(
                                from = v,
                                to = node_id,
                                type = "relevant",
                                condition = node_relevant,
                                stringsAsFactors = FALSE
                        )
                        edge_list[[length(edge_list) + 1]] <- edge_entry
                }
        }

        # Constraint edges
        if (!is.na(node_constraint) && node_constraint != "") {
                vars <- extract_variables(node_constraint)
                for (v in vars) {
                        cli::cli_alert_info("Adding constraint edge from '{v}' to '{node_id}' with condition '{node_constraint}'")
                        edge_entry <- tibble::tibble(
                                from = v,
                                to = node_id,
                                type = "constraint",
                                condition = node_constraint,
                                stringsAsFactors = FALSE
                        )
                        edge_list[[length(edge_list) + 1]] <- edge_entry
                }
        }

        # Calculation edges
        if (!is.na(node_calculation) && node_calculation != "") {
                vars <- extract_variables(node_calculation)
                for (v in vars) {
                        cli::cli_alert_info("Adding calculation edge from '{v}' to '{node_id}' with condition '{node_calculation}'")
                        edge_entry <- tibble::tibble(
                                from = v,
                                to = node_id,
                                type = "calculation",
                                condition = node_calculation,
                                stringsAsFactors = FALSE
                        )
                        edge_list[[length(edge_list) + 1]] <- edge_entry
                }
        }

        # Recurse for children if any
        if (length(node_children) > 0) {
                cli::cli_alert_info("Recursing into children of '{node_id}'")
                for (child in node_children) {
                        result <- flatten_hierarchy_recursive(child, parent_id = node_id, node_list, edge_list)
                        node_list <- c(node_list, result$nodes)
                        edge_list <- c(edge_list, result$edges)
                }
        }

        # Add a visual separator between processing of different nodes
        cli::cli_rule()

        return(list(nodes = node_list, edges = edge_list))
}
