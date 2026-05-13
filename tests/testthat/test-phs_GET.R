test_that("returns content", {
  skip_if_offline(host = "www.opendata.nhs.scot")

  content <- phs_GET("package_list", list())

  expect_true(content$success)

  expect_identical(
    content$help,
    "https://www.opendata.nhs.scot/api/3/action/help_show?name=package_list"
  )
})

test_that("error_check() works as expected", {
  skip_if_offline(host = "www.opendata.nhs.scot")

  # no error for valid endpoint
  expect_type(
    phs_GET("package_list", list()),
    "list"
  )

  # not found error
  expect_error(
    phs_GET("datastore_search", list(id = "doop"))
  )
})
