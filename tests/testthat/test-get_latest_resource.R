skip_if_offline(host = "www.opendata.nhs.scot")

test_that("returns data for a dataset that is listed", {
  expect_no_error(data <- get_latest_resource("general-practitioner-contact-details"))
  expect_s3_class(data, "tbl_df")
  expect_match(data[["ResName"]], "GP Details \\w+ [0-9]{4}")
  expect_named(data)
  expect_contains(names(data), c(
    "ResID", "ResName", "ResCreatedDate", "ResModifiedDate",
    "Surname", "Sex", "Postcode", "HB", "HSCP"
  ))
})

test_that("returns error for a dataset that is not listed", {
  expect_error(
    get_latest_resource("hospital-codes"),
    "not within the applicable datasets"
  )
})

test_that("errors for ambiguous resource", {
  expect_error(
    get_latest_resource_id("weekly-covid-19-statistical-data-in-scotland"),
    "The most recent id could not be identified"
  )
})
