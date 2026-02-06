skip_if_offline(host = "www.opendata.nhs.scot")

test_that("correctly extracts error from API response", {
  # Use phs_GET which now uses httr2
  # Use tryCatch because phs_GET calls error_check() which stops on error
  content <- tryCatch(
    phs_GET("datastore_search", "id=doop"),
    error = function(e) {
      return(NULL)
    } # We want to test parse_error on content
  )

  # Since phs_GET stops on error, let's call httr2 directly to get the content without stopping
  req <- httr2::request(request_url("datastore_search", "id=doop")) %>%
    httr2::req_error(is_error = function(resp) FALSE)
  resp <- httr2::req_perform(req)
  content <- httr2::resp_body_json(resp)

  expect_equal(
    parse_error(content$error),
    "Not Found Error: Not found: Resource \"doop\" was not found."
  )

  req <- httr2::request(request_url("datastore_search", "")) %>%
    httr2::req_error(is_error = function(resp) FALSE)
  resp <- httr2::req_perform(req)
  content <- httr2::resp_body_json(resp)

  expect_equal(
    parse_error(content$error),
    "resource_id: Missing value"
  )
})
