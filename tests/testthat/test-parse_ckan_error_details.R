test_that("parse_ckan_error_details handles general errors", {
  error <- list(
    message = "Not found: Resource \"doop\" was not found.",
    `__type` = "Not Found Error"
  )
  res <- parse_ckan_error_details(error)
  expect_equal(res$message, "Not Found Error: Not found: Resource \"doop\" was not found.")
  expect_null(res$bullets)
})

test_that("parse_ckan_error_details handles validation errors with bullets", {
  error <- list(
    message = "Validation Error",
    `__type` = "Validation Error",
    fields = list("Missing value"),
    q = list("Invalid query")
  )
  res <- parse_ckan_error_details(error)
  expect_equal(res$message, "Validation Error")
  expect_length(res$bullets, 2)
  expect_equal(unname(res$bullets), c("col_select: Missing value", "row_filters: Invalid query"))
  expect_equal(names(res$bullets), c("*", "*"))
})

test_that("parse_ckan_error_details handles SQL errors", {
  error <- list(
    message = "SQL Error",
    `__type` = "SQL Error",
    info = list(
      orig = list("^LINE 1: SELECT * FROM relation \"donut\"")
    )
  )
  res <- parse_ckan_error_details(error)
  # Strips caret and replaces LINE 1 with In SQL
  expect_equal(res$message, "In SQL: SELECT * FROM resource/table \"donut\"")
})
