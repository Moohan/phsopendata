test_that("add_context adds the correct information to a resource data frame", {
  res_id <- "test-id"
  res_name <- "test-name"
  res_created_date <- "2021-06-15T11:45:30"
  res_modified_date <- "2021-06-15T11:45:30"

  data <- tibble::tibble(col1 = 1)
  data_with_context <- add_context(
    data,
    res_id,
    res_name,
    res_created_date,
    res_modified_date
  )

  expect_true("ResID" %in% names(data_with_context))
  expect_true("ResName" %in% names(data_with_context))
  expect_true("ResCreatedDate" %in% names(data_with_context))
  expect_true("ResModifiedDate" %in% names(data_with_context))

  expect_equal(data_with_context$ResID[1], res_id)
  expect_equal(data_with_context$ResName[1], res_name)
  expect_s3_class(data_with_context$ResCreatedDate, "POSIXct")
  expect_s3_class(data_with_context$ResModifiedDate, "POSIXct")
})

test_that("add_context adds context to multiple resources", {
  skip_if_offline()

  dataset_name <- "gp-practice-populations"
  dataset <- get_dataset(dataset_name, max_resources = 2, include_context = TRUE)

  expect_s3_class(dataset, "tbl_df")
  expect_true("ResID" %in% names(dataset))
  expect_true("ResName" %in% names(dataset))
  expect_true("ResCreatedDate" %in% names(dataset))
  expect_true("ResModifiedDate" %in% names(dataset))

  # Ensure we have data from multiple resources
  expect_true(length(unique(dataset$ResID)) >= 1)
})
