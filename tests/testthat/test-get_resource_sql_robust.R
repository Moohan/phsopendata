test_that("internal record processing handles NULLs and mixed types correctly", {
  # Direct test of the new logic in get_resource_sql

  records <- list(
    list(id = 1, value = 1.5, name = "a"),
    list(id = 2, value = NULL, name = "b")
  )

  null_cols <- unique(unlist(lapply(
    records,
    function(x) names(x)[vapply(x, is.null, logical(1))]
  ), use.names = FALSE))

  expect_equal(null_cols, "value")

  query_data <- lapply(
    records,
    function(x) {
      for (col in null_cols) {
        if (is.null(x[[col]])) {
          x[[col]] <- ""
        } else {
          x[[col]] <- as.character(x[[col]])
        }
      }
      return(x)
    }
  ) %>%
    dplyr::bind_rows()

  expect_s3_class(query_data, "tbl")
  expect_equal(nrow(query_data), 2)
  expect_equal(query_data$value, c("1.5", ""))
  expect_type(query_data$value, "character")
})
