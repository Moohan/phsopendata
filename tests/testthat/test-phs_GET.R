test_that("returns httr::content", {
  skip_if_offline(host = "www.opendata.nhs.scot")

  content <- phs_GET("package_list", "")

  expect_true(content$success)

  expect_identical(
    content$help,
    "https://www.opendata.nhs.scot/api/3/action/help_show?name=package_list"
  )
})

test_that("phs_GET handles errors as expected", {
  skip_if_offline(host = "www.opendata.nhs.scot")

  # no error for valid endpoint
  expect_type(
    phs_GET("package_list", ""),
    "list"
  )

  # not found error with correct class and message
  expect_error(
    phs_GET("datastore_search", "id=doop"),
    regexp = 'Resource "doop" was not found.',
    class = "phsopendata_error_not_found"
  )

  # validation error with correct class
  expect_error(
    phs_GET("datastore_search", ""),
    regexp = "resource_id: Missing value",
    class = "phsopendata_error_validation"
  )
})

test_that("phs_GET fails for invalid action (offline test)", {
  # invalid action argument
  expect_error(
    phs_GET("", ""),
    regexp = "API call failed",
    class = "rlang_error"
  )
})
