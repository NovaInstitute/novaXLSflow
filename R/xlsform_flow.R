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
visualize_flow <- function(flowchart_data) {
        library(DiagrammeR)

        nodes <- flowchart_data$nodes
        edges <- flowchart_data$edges

        graph_code <- "digraph Flowchart {"

        node_ids <- nodes$id
        node_numbers <- setNames(seq_along(node_ids), node_ids)

        for (i in seq_along(node_ids)) {
                n_id <- node_ids[i]
                n <- nodes[nodes$id == n_id,]
                # label = "label (qtype)"
                label <- stringr::str_squish(paste0(n$label, " (", n$qtype, ")")) |>
                        str_replace_all(pattern = "'s ", replacement =  "s ")
                graph_code <- paste0(graph_code, "\n    ", node_numbers[[n_id]], " [label = \"", label, "\"];")
        }

        for (i in seq_len(nrow(edges))) {
                e <- edges[i,]
                if (e$from %in% node_ids && e$to %in% node_ids) {
                        from_num <- node_numbers[[e$from]]
                        to_num <- node_numbers[[e$to]]
                        if (!is.na(e$condition) && nzchar(e$condition)) {
                                graph_code <- paste0(graph_code, "\n    ", from_num, " -> ", to_num, " [label=\"", e$condition, "\"];")
                        } else {
                                graph_code <- paste0(graph_code, "\n    ", from_num, " -> ", to_num, ";")
                        }
                } else {
                        # If edge references missing nodes, let's produce a warning.
                        # The user mentioned it's dangerous to remove warnings and skip silently.
                        warning(paste("Edge references missing node:", e$from, "->", e$to, "Skipping edge."))
                }
        }

        graph_code <- paste0(graph_code, "\n}")
        diagram <- DiagrammeR::grViz(graph_code)
        DiagrammeR::is_property_graph(graph)
        diagram
}
