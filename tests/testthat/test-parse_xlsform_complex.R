# test-parse_xlsform_complex.R
library(testthat)
library(openxlsx)
library(dplyr)

test_that("parse_xlsform handles complex forms", {
        # Create a more complex XLSForm
        # This form includes:
        # - A group with two questions: one integer and one text.
        # - A repeat group with a calculation and a constraint.
        # - A top-level question dependent on a group's question.
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

        tmpfile <- tempfile(fileext = ".xlsx")
        wb <- createWorkbook()
        addWorksheet(wb, "survey")
        writeData(wb, "survey", survey_df)
        addWorksheet(wb, "choices")
        writeData(wb, "choices", choices_df)
        saveWorkbook(wb, tmpfile, overwrite = TRUE)

        flowchart_data <- parse_xlsform(tmpfile)

        # Basic checks
        expect_type(flowchart_data, "list")
        expect_named(flowchart_data, c("nodes", "edges"))

        # Check that all questions and groups appear as nodes
        # There should be 1 group node (personal_info), 2 questions inside it (age, notes),
        # 1 repeat group node (items_repeat) with item_count and total_calc,
        # and a top-level question (purchase).

        # total nodes: personal_info (group), age, notes, items_repeat (repeat), item_count, total_calc, purchase
        # total: 7 nodes
        expect_s3_class(flowchart_data$nodes, "data.frame")
        expect_true(all(c("id", "label", "qtype") %in% names(flowchart_data$nodes)))
        expect_equal(nrow(flowchart_data$nodes), 8)

        # Check some labels
        expect_equal(flowchart_data$nodes$label[flowchart_data$nodes$id == "age"], "What is your age?")
        expect_equal(flowchart_data$nodes$label[flowchart_data$nodes$id == "purchase"], "Will you purchase?")

        # Check edges
        # Relevant edge: from age to purchase (because ${age} >= 18)
        # Constraint edge: from item_count to itself (items_repeat), due to ${item_count} > 0
        # Calculation edge: from item_count to total_calc (sum(${item_count}))

        # At minimum:
        # relevant: age -> purchase
        # constraint: item_count -> item_count (or at least a node referencing itself, though constraint might be conceptual)
        # calculation: item_count -> total_calc
        # Also hierarchy edges for grouping
        # Let's ensure these exist:

        expect_s3_class(flowchart_data$edges, "data.frame")
        # Check that there's at least one edge referencing age to purchase
        rel_edge <- flowchart_data$edges %>% filter(type == "relevant", to == "purchase")
        expect_equal(nrow(rel_edge), 1)
        expect_equal(rel_edge$from, "age")

        # Check calculation edge
        calc_edge <- flowchart_data$edges %>% filter(type == "calculation", to == "total_calc")
        expect_equal(nrow(calc_edge), 1)
        expect_equal(calc_edge$from, "item_count")

        # Check constraint edge
        constraint_edge <- flowchart_data$edges %>% filter(type == "constraint", to == "item_count")
        expect_equal(nrow(constraint_edge), 1)
        expect_equal(constraint_edge$from, "item_count") # self-reference due to constraint

        # Check hierarchy edges: at least one hierarchy edge from root to personal_info, etc.
        hierarchy_edges <- flowchart_data$edges %>% filter(type == "hierarchy")
        expect_true(nrow(hierarchy_edges) > 0)
})
