# test-parse_xlsform.R
library(testthat)
library(openxlsx)
library(dplyr)

test_that("parse_xlsform works on a minimal example", {
        # Create a minimal XLSForm-like data frame
        survey_df <- data.frame(
                type = c("integer", "select_one yes_no", "text"),
                name = c("age", "smoker", "comments"),
                label = c("How old are you?", "Do you smoke?", "Any additional notes?"),
                relevant = c(NA, "${age} >= 18", NA),
                stringsAsFactors = FALSE
        )

        choices_df <- data.frame(
                list_name = c("yes_no", "yes_no"),
                name = c("yes", "no"),
                label = c("Yes", "No"),
                stringsAsFactors = FALSE
        )

        # Write these data frames to a temporary XLSX file using openxlsx
        tmpfile <- tempfile(fileext = ".xlsx")
        wb <- createWorkbook()
        addWorksheet(wb, "survey")
        writeData(wb, "survey", survey_df)
        addWorksheet(wb, "choices")
        writeData(wb, "choices", choices_df)
        saveWorkbook(wb, tmpfile, overwrite = TRUE)

        # Parse the XLSForm
        flowchart_data <- parse_xlsform(tmpfile)

        # Test structure of output
        expect_type(flowchart_data, "list")
        expect_named(flowchart_data, c("nodes", "edges"))

        # Test nodes
        expect_s3_class(flowchart_data$nodes, "data.frame")
        expect_equal(nrow(flowchart_data$nodes), 3)
        expect_true(all(c("id", "label", "qtype") %in% names(flowchart_data$nodes)))

        # Check a node's label
        expect_equal(flowchart_data$nodes$label[flowchart_data$nodes$id == "age"], "How old are you?")

        # Test edges
        expect_s3_class(flowchart_data$edges, "data.frame")
        # There should be one edge due to the relevant condition
        expect_equal(nrow(flowchart_data$edges), 1)
        expect_true(all(c("from", "to", "type", "condition") %in% names(flowchart_data$edges)))
        expect_equal(flowchart_data$edges$from, "age")
        expect_equal(flowchart_data$edges$to, "smoker")
        expect_equal(flowchart_data$edges$type, "relevant")
        expect_equal(flowchart_data$edges$condition, "${age} >= 18")
})
