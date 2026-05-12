skip_if_offline(host = "www.opendata.nhs.scot")

test_that("correctly extracts error from API response", {
  # Note: request_url now returns an httr2_request.
  # We use req_error(is_error = ~ FALSE) to prevent it from throwing on 404
  # so we can inspect the body manually for this test.

  content <- request_url("datastore_search", list(id = "doop")) |>
    httr2::req_error(is_error = \(resp) FALSE) |>
    httr2::req_perform() |>
    httr2::resp_body_json()

  expect_identical(
    parse_error(content$error),
    "Not Found Error: Not found: Resource \"doop\" was not found."
  )

  content <- request_url("datastore_search", list()) |>
    httr2::req_error(is_error = \(resp) FALSE) |>
    httr2::req_perform() |>
    httr2::resp_body_json()

  expect_identical(
    parse_error(content$error),
    "resource_id: Missing value"
  )
})
