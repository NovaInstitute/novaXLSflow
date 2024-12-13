test_that("parse_xlsform parses the XLSForm correctly", {
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
        wb <- createWorkbook()
        addWorksheet(wb, "survey")
        writeData(wb, "survey", survey_df)
        addWorksheet(wb, "choices")
        writeData(wb, "choices", choices_df)
        saveWorkbook(wb, tmpfile, overwrite = TRUE)

        # Parse the XLSForm
        flowchart_data <- parse_xlsform(tmpfile)

        # Expected nodes
        expected_nodes <- tibble::tibble(
                id = c("root","personal_info","age",
                       "notes","items_repeat","item_count","total_calc",
                       "purchase"),
                label = c("root","Personal Information",
                          "What is your age?","Any notes?","Items",
                          "How many items?","sum(${item_count})","Will you purchase?"),
                qtype = c("root","begin_group",
                          "integer","text","begin_repeat","integer","calculate",
                          "select_one yes_no"),
                stringsAsFactors = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)
        )

        # Expected edges
        expected_edges <- tibble::tibble(
                from = c("root","personal_info",
                         "personal_info","root","items_repeat","item_count",
                         "items_repeat","item_count","root","age"),
                to = c("personal_info","age","notes",
                       "items_repeat","item_count","item_count",
                       "total_calc","total_calc","purchase","purchase"),
                type = c("hierarchy","hierarchy",
                         "hierarchy","hierarchy","hierarchy","constraint",
                         "hierarchy","calculation","hierarchy","relevant"),
                condition = c(NA,NA,NA,NA,NA,
                              "${item_count} > 0",NA,"sum(${item_count})",NA,"${age} >= 18"),
                stringsAsFactors = c(FALSE,FALSE,FALSE,FALSE,
                                     FALSE,FALSE,FALSE,FALSE,FALSE,FALSE)
        )

        # Test nodes
        expect_equal(flowchart_data$nodes, expected_nodes)

        # Test edges
        expect_equal(flowchart_data$edges, expected_edges)
})
