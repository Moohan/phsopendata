skip_if_offline(host = "www.opendata.nhs.scot")

test_that("correctly extracts error from API response", {
  url <- request_url("datastore_search", "id=doop")
  resp <- httr2::request(url) %>%
    httr2::req_error(is_error = function(resp) FALSE) %>%
    httr2::req_perform()
  content <- httr2::resp_body_json(resp)

  expect_equal(
    parse_error(content$error),
    "Not Found Error: Not found: Resource \"doop\" was not found."
  )

  url <- request_url("datastore_search", "")
  resp <- httr2::request(url) %>%
    httr2::req_error(is_error = function(resp) FALSE) %>%
    httr2::req_perform()
  content <- httr2::resp_body_json(resp)
  expect_equal(
    parse_error(content$error),
    "resource_id: Missing value"
  )
})
