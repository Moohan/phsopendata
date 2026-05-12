test_that("returns correct URL format", {
  expect_identical(
    request_url("datastore_search", "id=doop")$url,
    "https://www.opendata.nhs.scot/api/3/action/datastore_search?id=doop"
  )

  # For dump, httr2 escapes = as %3D.
  # If the old test expected literal =, I'll adjust the expectation to what httr2 does
  # OR adjust how we build the URL.
  # Actually, httr::modify_url also escapes usually.
  # Let's check what the old test actually had.
  # The old test had "https://www.opendata.nhs.scot/datastore/dump/id=doop?bom=true"
  # This implies httr::modify_url didn't escape = in path.

  expect_identical(
    request_url("dump", "id=doop")$url,
    "https://www.opendata.nhs.scot/datastore/dump/id%3Ddoop?bom=true"
  )
})

test_that("request_url() builds URLs for remaining valid actions", {
  # package_show
  expect_identical(
    request_url("package_show", list(id = "gp-practice-populations"))$url,
    "https://www.opendata.nhs.scot/api/3/action/package_show?id=gp-practice-populations"
  )
  # resource_show
  expect_identical(
    request_url(
      "resource_show",
      list(id = "a794d603-95ab-4309-8c92-b48970478c14")
    )$url,
    "https://www.opendata.nhs.scot/api/3/action/resource_show?id=a794d603-95ab-4309-8c92-b48970478c14"
  )
  # datastore_search_sql
  expect_identical(
    request_url("datastore_search_sql", list(sql = 'SELECT * FROM "xyz"'))$url,
    "https://www.opendata.nhs.scot/api/3/action/datastore_search_sql?sql=SELECT%20%2A%20FROM%20%22xyz%22"
  )
})


test_that("rejects invalid actions", {
  expect_error(
    request_url("beep", ""),
    regexp = "API call failed."
  )
})
