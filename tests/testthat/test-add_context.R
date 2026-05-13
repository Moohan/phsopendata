test_that("add_context adds the correct information to a resource data frame", {
  res_id <- "ca3f8e44-9a84-43d6-819c-a880b23bd278"
  res_name <- "GP Practice Populations HB2019 June 2021"
  res_created_date <- "2021-06-15T11:45:30"
  res_modified_date <- "2021-06-15T11:45:30"

  data <- get_resource(res_id, rows = 1)
  data_with_context <- add_context(
    data,
    res_id,
    res_name,
    res_created_date,
    res_modified_date
  )

  expect_true("ResID" %in% names(data_with_context))
  expect_true("ResName" %in% names(data_with_context))
  expect_true("DateCreated" %in% names(data_with_context))
  expect_true("LastUpdated" %in% names(data_with_context))

  expect_equal(data_with_context$ResID[1], res_id)
  expect_equal(data_with_context$ResName[1], res_name)
})

test_that("add_context adds context to multiple resources", {
  res_id_1 <- "ca3f8e44-9a84-43d6-819c-a880b23bd278"
  res_id_2 <- "8bc3571d-5569-42b7-a89e-21447239ef9b"

  dataset <- get_dataset("gp-practice-populations", max_resources = 2, include_context = TRUE)

  data_1 <- dplyr::filter(dataset, ResID == res_id_1)
  data_2 <- dplyr::filter(dataset, ResID == res_id_2)

  expect_equal(unique(data_1$ResID), res_id_1)
  expect_equal(unique(data_2$ResID), res_id_2)
})
