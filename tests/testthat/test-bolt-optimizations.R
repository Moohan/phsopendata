library(testthat)
library(dplyr)
library(tibble)

# Mock phsopendata internal functions and others if needed
# Since I'm using pkgload::load_all(), I can just use the functions directly

test_that("Vectorized type check works as expected", {
  # Case 1: All same types
  all_data <- list(
    tibble(A = 1:2, B = letters[1:2]),
    tibble(A = 3:4, B = letters[3:4])
  )

  # I'll call the internal logic by copying it or using the package function if it's exported/accessible
  # Since it's not exported, I'll use phsopendata:::get_dataset but that involves API calls.
  # Better to test the logic directly here.

  check_types <- function(all_data) {
    all_names <- unlist(lapply(all_data, names), use.names = FALSE)
    all_types <- unlist(
      lapply(
        all_data,
        function(df) vapply(df, function(x) class(x)[1L], character(1L))
      ),
      use.names = FALSE
    )
    types_by_name <- split(all_types, all_names)
    names(types_by_name)[
      vapply(types_by_name, function(x) length(unique(x)) > 1L, logical(1L))
    ]
  }

  expect_equal(check_types(all_data), character(0))

  # Case 2: Inconsistent types
  all_data_inc <- list(
    tibble(A = 1:2, B = letters[1:2]),
    tibble(A = "3", B = letters[3:4])
  )
  expect_equal(check_types(all_data_inc), "A")

  # Case 3: Multiple inconsistencies
  all_data_inc2 <- list(
    tibble(A = 1:2, B = 1:2),
    tibble(A = "3", B = "4")
  )
  expect_setequal(check_types(all_data_inc2), c("A", "B"))

  # Case 4: Non-overlapping columns
  all_data_non_overlap <- list(
    tibble(A = 1:2),
    tibble(B = 1:2)
  )
  expect_equal(check_types(all_data_non_overlap), character(0))
})

test_that("Vectorized context addition works as expected", {
  all_data <- list(
    tibble(Val = 1:2),
    tibble(Val = 3:5)
  )
  selection_ids <- c("id1", "id2")
  res_names <- c("name1", "name2")
  created_dates <- c("2023-01-01T10:00:00", "2023-01-02T10:00:00")
  modified_dates <- c("2023-01-01T11:00:00", "2023-01-02T11:00:00")

  # Simulate get_dataset's vectorized addition
  combined <- purrr::list_rbind(all_data)
  rows_per_res <- vapply(all_data, nrow, integer(1L))

  ids_expanded <- rep(selection_ids, rows_per_res)
  names_expanded <- rep(res_names, rows_per_res)

  created_parsed <- as.POSIXct(created_dates, format = "%FT%X", tz = "UTC")
  modified_parsed <- as.POSIXct(modified_dates, format = "%FT%X", tz = "UTC")

  created_expanded <- rep(created_parsed, rows_per_res)
  modified_expanded <- rep(modified_parsed, rows_per_res)

  # Use phsopendata::add_context
  result <- phsopendata:::add_context(
    data = combined,
    id = ids_expanded,
    name = names_expanded,
    created_date = created_expanded,
    modified_date = modified_expanded
  )

  expect_equal(nrow(result), 5)
  expect_equal(result$ResID, c("id1", "id1", "id2", "id2", "id2"))
  expect_equal(result$ResName, c("name1", "name1", "name2", "name2", "name2"))
  expect_equal(result$ResCreatedDate[1], created_parsed[1])
  expect_equal(result$ResCreatedDate[3], created_parsed[2])
})
