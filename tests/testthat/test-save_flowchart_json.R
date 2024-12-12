# test-save_flowchart_json.R
library(testthat)
library(jsonlite)

test_that("save_flowchart_json creates a valid JSON file", {
        # Create a mock flowchart_data
        flowchart_data <- list(
                nodes = data.frame(
                        id = c("age", "smoker"),
                        label = c("How old are you?", "Do you smoke?"),
                        qtype = c("integer", "select_one"),
                        stringsAsFactors = FALSE
                ),
                edges = data.frame(
                        from = c("age"),
                        to = c("smoker"),
                        type = c("relevant"),
                        condition = c("${age} >= 18"),
                        stringsAsFactors = FALSE
                )
        )

        tmp_json <- tempfile(fileext = ".json")
        save_flowchart_json(flowchart_data, tmp_json)

        # Check that file exists
        expect_true(file.exists(tmp_json))

        # Check JSON validity
        parsed <- jsonlite::fromJSON(tmp_json)
        expect_true(is.list(parsed))
        expect_named(parsed, c("nodes", "edges"))

        # Check contents
        expect_equal(parsed$nodes$id, c("age", "smoker"))
        expect_equal(parsed$edges$from, "age")
})
