#' Parse an XLSForm file into a JSON flowchart structure
#'
#' @param xls_path A character string path to the XLSForm file (Excel file).
#' @return A list containing nodes and edges representing the form logic.
#' @import openxlsx dplyr stringr jsonlite
#' @export
parse_xlsform <- function(xls_path) {
        survey <- openxlsx::read.xlsx(xls_path, sheet = "survey")

        # Normalize column names to lowercase
        names(survey) <- tolower(names(survey))

        # Check required columns
        required_cols <- c("type", "name", "label")
        missing_cols <- setdiff(required_cols, names(survey))
        if (length(missing_cols) > 0) {
                stop("Missing required columns in `survey` sheet: ", paste(missing_cols, collapse = ", "))
        }

        # Extract nodes
        nodes <- survey %>%
                dplyr::mutate(
                        id = name,
                        label = ifelse(is.na(label), name, label),
                        qtype = type
                ) %>%
                dplyr::select(id, label, qtype)

        # Function to extract variables from a relevant expression
        extract_variables <- function(expr) {
                if (is.na(expr)) return(character(0))
                matches <- stringr::str_match_all(expr, "\\$\\{([^}]+)\\}")
                unique(matches[[1]][,2]) # return variable names found inside ${...}
        }

        edges_list <- list()

        for (i in seq_len(nrow(survey))) {
                rel_expr <- survey$relevant[i]
                if (!is.na(rel_expr)) {
                        vars <- extract_variables(rel_expr)
                        for (v in vars) {
                                if (v %in% nodes$id) {
                                        edges_list[[length(edges_list) + 1]] <- list(
                                                from = v,
                                                to = survey$name[i],
                                                type = "relevant",
                                                condition = rel_expr
                                        )
                                } else {
                                        warning("Relevant condition references unknown variable: ", v)
                                }
                        }
                }
        }

        edges <- if (length(edges_list) > 0) {
                do.call(rbind, lapply(edges_list, as.data.frame))
        } else {
                data.frame(from = character(0), to = character(0), type = character(0), condition = character(0))
        }

        list(nodes = nodes, edges = edges)
}

#' Save flowchart data as JSON
#'
#' @param flowchart_json A list with elements `nodes` and `edges` as produced by `parse_xlsform`.
#' @param out_file The file path to save the JSON to.
#' @return Invisible NULL
#' @importFrom jsonlite write_json
#' @export
save_flowchart_json <- function(flowchart_json, out_file = "flowchart.json") {
        jsonlite::write_json(flowchart_json, out_file, pretty = TRUE)
        message("Flowchart JSON saved to ", out_file)
        invisible(NULL)
}

#' Visualize XLSForm flowchart using DiagrammeR
#'
#' @param flowchart_json A list with elements `nodes` and `edges` as produced by `parse_xlsform`.
#' @return A DiagrammeR graph object.
#' @importFrom DiagrammeR DiagrammeR
#' @export
visualize_flow <- function(flowchart_json) {
        nodes <- flowchart_json$nodes
        edges <- flowchart_json$edges

        # Map nodes to numeric IDs
        node_map <- data.frame(
                id_num = seq_len(nrow(nodes)),
                id = nodes$id,
                stringsAsFactors = FALSE
        )

        # Build node statements
        node_statements <- apply(node_map, 1, function(row) {
                label_txt <- nodes$label[nodes$id == row[2]]
                qtype_txt <- nodes$qtype[nodes$id == row[2]]
                paste0(row[1], " [label = \"", label_txt, " (", qtype_txt, ")\"]")
        })

        # Build edge statements
        edge_statements <- character(0)
        if (nrow(edges) > 0) {
                edge_statements <- apply(edges, 1, function(e) {
                        from_id <- node_map$id_num[node_map$id == e["from"]]
                        to_id <- node_map$id_num[node_map$id == e["to"]]
                        cond <- if (!is.na(e["condition"]) && nchar(e["condition"]) > 0) {
                                paste0(" [label=\"", e["condition"], "\"]")
                        } else {
                                ""
                        }
                        paste0(from_id, " -> ", to_id, cond)
                })
        }

        graph_code <- paste0(
                "digraph xlsform {\n",
                "  rankdir=LR;\n",
                "  ", paste(node_statements, collapse = "\n  "), "\n\n",
                "  ", paste(edge_statements, collapse = "\n  "), "\n",
                "}"
        )

        # Use grViz to return a grViz htmlwidget
        DiagrammeR::grViz(graph_code)
}
