skip_if_offline(host = "www.opendata.nhs.scot")

test_that("correctly extracts error from API response", {
  content <- httr2::request(request_url("datastore_search", "id=doop")) %>%
    httr2::req_error(is_error = function(x) FALSE) %>%
    httr2::req_perform() %>%
    httr2::resp_body_json()

  expect_equal(
    parse_error(content$error),
    "Not Found Error: Not found: Resource \"doop\" was not found."
  )

  content <- httr2::request(request_url("datastore_search", "")) %>%
    httr2::req_error(is_error = function(x) FALSE) %>%
    httr2::req_perform() %>%
    httr2::resp_body_json()

  expect_equal(
    parse_error(content$error),
    "resource_id: Missing value"
  )
})
