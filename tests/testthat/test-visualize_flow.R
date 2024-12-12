test_that("visualize_flow returns a DiagrammeR graph", {
        # Mock flowchart data
        flowchart_data <- list(
                nodes = data.frame(
                        id = c("age", "smoker"),
                        label = c("How old are you?", "Do you smoke?"),
                        qtype = c("integer", "select_one"),
                        stringsAsFactors = FALSE
                ),
                edges = data.frame(
                        from = "age",
                        to = "smoker",
                        type = "relevant",
                        condition = "${age} >= 18",
                        stringsAsFactors = FALSE
                )
        )

        graph <- visualize_flow(flowchart_data)

        # Check that the return is a grViz (which also inherits from htmlwidget)
        expect_s3_class(graph, "grViz")

        # Inspect the DOT code
        dot_code <- graph$x$diagram
        expect_true(grepl("How old are you", dot_code))
        expect_true(grepl("Do you smoke", dot_code))
        expect_true(grepl("->", dot_code))
})
