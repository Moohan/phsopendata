test_that("use_dump_check returns TRUE if rows is NULL", {
  query <- list(id = "res_id")
  rows <- NULL
  expect_true(use_dump_check(query, rows))
})

test_that("use_dump_check returns FALSE if rows is not NULL", {
  query <- list(id = "res_id")
  rows <- 100
  expect_false(use_dump_check(query, rows))
})

test_that("use_dump_check returns FALSE if filters are present", {
  query <- list(id = "res_id", q = "filter")
  rows <- NULL
  expect_false(use_dump_check(query, rows))
})
