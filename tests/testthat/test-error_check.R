skip_if_offline(host = "www.opendata.nhs.scot")

test_that("returns nothing if no error", {
  expect_null(
    error_check(phs_GET("package_list", list()))
  )
})

test_that("throws error if error in content", {
  # We use tryCatch because phs_GET itself will throw an error now due to req_error()
  # But here we want to test error_check directly if possible, or via phs_GET
  expect_error(
    phs_GET("datastore_search", list(id = "doop")),
    regexp = 'Resource "doop" was not found.'
  )
})
