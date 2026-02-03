test_that("returns data for a dataset that is listed", {
  expect_no_error(get_latest_resource("general-practitioner-contact-details"))
})

test_that("returns error for a dataset that is not listed", {
  expect_error(get_latest_resource("hospital-codes"))
})
