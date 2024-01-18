skip_if_offline("www.opendata.nhs.scot")

test_that("lists datasets", {
  datasets <- list_datasets()

  expect_type(datasets, "character")

  expect_gt(length(datasets), 0)
})
