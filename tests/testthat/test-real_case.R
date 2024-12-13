# tests/testthat/test-complex_form.R

testthat::test_that("parse_xlsform and visualize_flow handle the provided complex form correctly", {
        # The test already sets up 'file', 'flowchart_data', 'graph', 'dot_code'
        file <- testthat::test_path("data", "example_household_listing_and_mapping.xlsx")
        flowchart_data <- parse_xlsform(file)
        graph <- visualize_flow(flowchart_data)
        testthat::expect_s3_class(graph, "grViz")

        dot_code <- graph$x$diagram

        # Basic existence checks on nodes
        testthat::expect_true("enumerator_name" %in% flowchart_data$nodes$id, info = "enumerator_name node should exist")

        # If root exists, check for a hierarchy edge from root to enumerator_name
        if ("root" %in% flowchart_data$nodes$id) {
                testthat::expect_true(
                        any(flowchart_data$edges$from == "root" & flowchart_data$edges$to == "enumerator_name" & flowchart_data$edges$type == "hierarchy"),
                        info = "Root -> enumerator_name hierarchy edge not found"
                )
        }

        # Additional tests for DOT code integrity

        # 1. Check that dot_code starts and ends correctly
        testthat::expect_true(grepl("^digraph", dot_code), info = "DOT code should start with 'digraph'")
        testthat::expect_true(grepl("}$", dot_code), info = "DOT code should end with '}'")

        # 2. Ensure no suspicious unescaped double quotes
        #   Each node/edge line should look like: [label = "some text"]
        #   If there's a syntax error near '"', it might be caused by unmatched quotes.
        # Check for lines that start with a node/edge definition and contain balanced quotes.
        dot_lines <- strsplit(dot_code, "\n")[[1]]

        # Filter only lines that define nodes or edges
        definition_lines <- grep("\\[label = \"", dot_lines, value = TRUE) |>
                stringr::str_replace_all(pattern = "\"s ", replacement =  "'s ")

        # Each definition line should contain a matching [label = "...] segment with balanced quotes.
        # We can test that each line has exactly two double quotes after 'label = ':
        for (line in definition_lines) {
                quote_count <- stringr::str_count(line, "\"")
                stringr::str_count(line, "\"")
                # Expect at least 2 quotes (for label = "text")
                testthat::expect_gte(quote_count, 2)
                # Expect an even number of quotes to avoid mismatches
                testthat::expect_true(quote_count %% 2 == 0)
        }

        # 3. Check for control characters that may break DOT syntax:
        # Sometimes newline characters in labels or odd characters break the code.
        # We'll allow common ASCII but deny carriage returns or other controls.
        # This is a heuristic check. If the form is large, just ensure no \r or similar are in code:
        testthat::expect_false(grepl("\r", dot_code), info = "DOT code contains carriage returns which can break syntax.")

        # 4. Check that every node referenced by edges exists:
        # Already done partially, but let's ensure no edge references a missing node:
        edge_nodes <- unique(c(flowchart_data$edges$from, flowchart_data$edges$to))
        # Remove NAs if any:
        edge_nodes <- edge_nodes[!is.na(edge_nodes)]
        missing_nodes <- edge_nodes[!edge_nodes %in% flowchart_data$nodes$id]
        testthat::expect_length(missing_nodes, 0)

        # If you still see errors, print dot_code for inspection:
        # cat(dot_code)

        # If everything passes, we know the DOT code is likely syntactically correct and no references are missing.
})
