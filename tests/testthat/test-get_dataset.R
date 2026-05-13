test_that("returns data with expected type and non-zero rows", {
  skip_if_offline(host = "www.opendata.nhs.scot")

  data <- get_dataset("gp-practice-populations", max_resources = 1L)

  expect_s3_class(data, "tbl_df")
  expect_gt(nrow(data), 0L)
  expect_true("PracticeCode" %in% names(data))
})

test_that("works with max_resources argument", {
  skip_if_offline(host = "www.opendata.nhs.scot")

  data <- get_dataset("gp-practice-populations", max_resources = 1L, include_context = TRUE)

  # ensure data is from only 1 resource
  expect_identical(
    length(unique(data[["ResName"]])),
    1L
  )
})

test_that("get_dataset works when 0 resources match criteria", {
  skip_if_offline(host = "www.opendata.nhs.scot")

  # use col_select to ensure the returned columns are correctly added
  # (even if there are 0 rows)
  data <- get_dataset(
    "gp-practice-populations",
    row_filters = list(PracticeCode = "NOTAREALCODE"),
    col_select = c("PracticeCode", "HSCP"),
    max_resources = 1L
  )

  expect_s3_class(data, "tbl_df")
  expect_identical(nrow(data), 0L)
  expect_identical(names(data), c("PracticeCode", "HSCP"))
})

test_that("get_dataset filters error properly", {
  expect_error(
    get_dataset("gp-practice-populations", col_select = "Non-existent column")
  )
})

test_that("get_dataset works with multiple filters", {
  n_resources <- 3L
  columns <- c("Date", "PracticeCode", "HSCP", "AllAges")

  data <- get_dataset(
    "gp-practice-populations",
    max_resources = n_resources,
    col_select = columns,
    include_context = TRUE
  )

  expect_identical(
    length(unique(data[["ResName"]])),
    n_resources
  )

  expect_identical(
    sort(names(data)),
    sort(c("ResID", "ResName", "ResCreatedDate", "ResModifiedDate", columns))
  )
})
