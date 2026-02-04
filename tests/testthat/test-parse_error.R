skip_if_offline(host = "www.opendata.nhs.scot")

test_that("correctly extracts error from API response", {
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
