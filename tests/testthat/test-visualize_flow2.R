# tests/testthat/test-visualize_flow2.R
testthat::test_that("parse_xlsform and visualize_flow integration works correctly for complex XLSForms", {
        # Define the XLSForm data
        survey_df <- data.frame(
                type = c("begin_group", "integer", "text", "end_group",
                         "begin_repeat", "integer", "calculate", "end_repeat",
                         "select_one yes_no"),
                name = c("personal_info", "age", "notes", "personal_info_end",
                         "items_repeat", "item_count", "total_calc", "items_repeat_end",
                         "purchase"),
                label = c("Personal Information", "What is your age?", "Any notes?", NA,
                          "Items", "How many items?", NA, NA,
                          "Will you purchase?"),
                relevant = c(NA, NA, NA, NA,
                             NA, NA, NA, NA,
                             "${age} >= 18"),
                constraint = c(NA, NA, NA, NA,
                               NA, "${item_count} > 0", NA, NA,
                               NA),
                calculation = c(NA, NA, NA, NA,
                                NA, NA, "sum(${item_count})", NA,
                                NA),
                stringsAsFactors = FALSE
        )

        choices_df <- data.frame(
                list_name = c("yes_no", "yes_no"),
                name = c("yes", "no"),
                label = c("Yes", "No"),
                stringsAsFactors = FALSE
        )

        # Create a temporary XLSX file
        tmpfile <- tempfile(fileext = ".xlsx")
        wb <- openxlsx::createWorkbook()
        openxlsx::addWorksheet(wb, "survey")
        openxlsx::writeData(wb, "survey", survey_df)
        openxlsx::addWorksheet(wb, "choices")
        openxlsx::writeData(wb, "choices", choices_df)
        openxlsx::saveWorkbook(wb, tmpfile, overwrite = TRUE)

        # Parse the XLSForm
        flowchart_data <- novaXLSflow::parse_xlsform(tmpfile)

        # Visualize the flow
        graph <- novaXLSflow::visualize_flow(flowchart_data)

        # Check that the graph is a grViz object
        testthat::expect_s3_class(graph, "grViz")

        # Extract the DOT code
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
                        stringr::str_detect(dot_code, label),
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

        edges_extracted <- tibble(bind_rows(edges_extracted)) |>
                mutate(condition = na_if(condition, ""))


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
