# tests/testthat/test-visualize_flow.R

testthat::test_that("visualize_flow returns a DiagrammeR graph with correctly formatted labels and edges", {
        # Mock flowchart data including 'root' node
        flowchart_data <- list(
                nodes = data.frame(
                        id = c("root", "age", "purchase"),
                        label = c("root", "What is your age?", "Will you purchase?"),
                        qtype = c("root", "integer", "select_one yes_no"),
                        stringsAsFactors = FALSE
                ),
                edges = data.frame(
                        from = c("age"),
                        to = c("purchase"),
                        type = c("relevant"),
                        condition = c("${age} >= 18"),
                        stringsAsFactors = FALSE
                )
        )

        graph <- novaXLSflow::visualize_flow(flowchart_data)

        # Check that the return is a grViz object
        testthat::expect_s3_class(graph, "grViz")

        # Inspect the DOT code
        dot_code <- graph$x$diagram

        # Step 1: Extract node definitions from dot_code
        node_definitions <- strsplit(dot_code, "\n")[[1]]
        node_lines <- grep("^\\s*\\d+ \\[label = \"", node_definitions, value = TRUE)

        # Extract node number and label_qtype
        nodes_extracted <- lapply(node_lines, function(line) {
                # Use regex to extract number and label
                matches <- regmatches(line, regexec("^\\s*(\\d+) \\[label = \"(.+)\"\\]", line))[[1]]
                if (length(matches) == 3) {
                        return(list(
                                id_num = as.integer(matches[2]),
                                label_qtype = matches[3]
                        ))
                } else {
                        return(NULL)
                }
        })

        nodes_extracted <- do.call(rbind, lapply(nodes_extracted, as.data.frame))
        colnames(nodes_extracted) <- c("id_num", "label_qtype")
        nodes_extracted$id_num <- as.integer(as.character(nodes_extracted$id_num))
        nodes_extracted$label_qtype <- as.character(nodes_extracted$label_qtype)

        # Step 2: Create expected_nodes with label_qtype
        expected_nodes <- flowchart_data$nodes %>%
                dplyr::mutate(label_qtype = paste0(label, " (", qtype, ")"))

        # Step 3: Join nodes_extracted with expected_nodes to get node_id
        node_map <- nodes_extracted %>%
                dplyr::inner_join(expected_nodes, by = "label_qtype") %>%
                dplyr::select(id_num, id)

        # Step 4: Verify node labels
        expected_node_labels <- expected_nodes$label_qtype |>
                stringr::str_escape()




        # Verify that each expected label is present in the DOT code
        for (label in expected_node_labels) {
                testthat::expect_true(
                        str_detect(dot_code, label),
                        # str_detect(dot_code, digraph Flowchart)
                        info = paste("Label not found:", label)
                )
        }

        # Step 5: Extract edge definitions from dot_code
        edge_definitions <- grep("^\\s*\\d+ -> \\d+", node_definitions, value = TRUE)

        # Parse edges into a data frame
        edges_extracted <- lapply(edge_definitions, function(line) {
                # Use regex to extract from, to, and optional label
                matches <- regmatches(line, regexec("^\\s*(\\d+) -> (\\d+)( \\[label=\"(.+)\"\\])?", line))[[1]]
                if (length(matches) >= 3) {
                        from_num <- as.integer(matches[2])
                        to_num <- as.integer(matches[3])
                        condition <- ifelse(length(matches) >= 5, matches[5], NA)
                        return(data.frame(from_num, to_num, condition, stringsAsFactors = FALSE))
                } else {
                        return(NULL)
                }
        })

        edges_extracted <- do.call(rbind, lapply(edges_extracted, as.data.frame))
        colnames(edges_extracted) <- c("from_num", "to_num", "condition")

        # Step 6: Create a lookup for node ID to number
        id_to_num <- setNames(node_map$id_num, node_map$id)

        # Step 7: Define expected edges based on flowchart_data
        expected_edges <- flowchart_data$edges %>%
                dplyr::mutate(
                        from_num = id_to_num[from],
                        to_num = id_to_num[to]
                )

        # Step 8: Verify each expected edge exists in edges_extracted
        for (i in seq_len(nrow(expected_edges))) {
                edge <- expected_edges[i, ]
                # Find matching edge in edges_extracted
                matching_edge <- edges_extracted %>%
                        dplyr::filter(from_num == edge$from_num, to_num == edge$to_num)

                if (!is.na(edge$condition) && nzchar(edge$condition)) {
                        # Edge with condition
                        expected_label <- edge$condition
                        condition_match <- any(matching_edge$condition == expected_label)
                        testthat::expect_true(
                                condition_match,
                                info = paste("Edge not found or incorrect:",
                                             edge$from, "->", edge$to,
                                             "with condition", edge$condition)
                        )
                } else {
                        # Edge without condition
                        condition_match <- any(is.na(matching_edge$condition))
                        testthat::expect_true(
                                condition_match,
                                info = paste("Edge not found:", edge$from, "->", edge$to)
                        )
                }
        }
})
