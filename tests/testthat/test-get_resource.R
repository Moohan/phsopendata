test_that("get_resource correctly identifying if columns have been selected", {
  res_id <- "ca3f8e44-9a84-43d6-819c-a880b23bd278"
  col_select <- c("HB", "Month")
  data <- get_resource(res_id, col_select = col_select, rows = 1)

  expect_equal(names(data), col_select)
})
