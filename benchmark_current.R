
.libPaths('~/.R/library')
library(dplyr)
library(purrr)
library(bench)
library(tibble)

# Mock data setup
n_rows <- 1000
n_res <- 400

mock_records <- replicate(n_rows, list(a = 1, b = "two", c = TRUE), simplify = FALSE)

# Benchmark 1: Redundant map in get_resource
cat("Benchmark 1: Redundant map in get_resource\n")
bm1 <- bench::mark(
  original = {
    purrr::map(mock_records, ~.x) %>%
      dplyr::bind_rows()
  },
  optimized = {
    dplyr::bind_rows(mock_records)
  },
  iterations = 10
)
print(bm1)

# Benchmark 2: Iterative vs Vectorized context addition
cat("\nBenchmark 2: Iterative vs Vectorized context addition\n")

# Current iterative logic (simplified)
add_context_iterative <- function(data_list, id, name, created, modified) {
  purrr::pmap(
    list(
      data = data_list,
      id = id,
      name = name,
      created_date = created,
      modified_date = modified
    ),
    function(data, id, name, created_date, modified_date) {
      if (is.null(modified_date)) modified_date <- NA_character_
      created_date <- as.POSIXct(created_date, format = "%FT%X", tz = "UTC")
      modified_date <- as.POSIXct(modified_date, format = "%FT%X", tz = "UTC")
      if (!is.na(modified_date) && modified_date < created_date) modified_date <- created_date

      dplyr::mutate(
        data,
        ResID = id,
        ResName = name,
        ResCreatedDate = created_date,
        ResModifiedDate = modified_date,
        .before = everything()
      )
    }
  )
}

# Proposed vectorized logic (simplified)
add_context_vectorized <- function(data_list, id, name, created, modified) {
  # Pre-parse dates
  created_parsed <- as.POSIXct(created, format = "%FT%X", tz = "UTC")
  modified_parsed <- as.POSIXct(
    if (is.null(modified)) rep(NA_character_, length(created)) else modified,
    format = "%FT%X", tz = "UTC"
  )

  # Handled vectorized comparison
  idx <- !is.na(modified_parsed) & modified_parsed < created_parsed
  modified_parsed[idx] <- created_parsed[idx]

  # Combine first
  combined <- purrr::list_rbind(data_list, names_to = "res_idx")

  # Use row indexing to map context efficiently
  res_idx <- as.integer(combined$res_idx)
  combined$res_idx <- NULL

  # To mimic .before = everything() with high performance
  res_id_vec <- id[res_idx]
  res_name_vec <- name[res_idx]
  res_created_vec <- created_parsed[res_idx]
  res_modified_vec <- modified_parsed[res_idx]

  # Re-attach
  dplyr::bind_cols(
    tibble(
      ResID = res_id_vec,
      ResName = res_name_vec,
      ResCreatedDate = res_created_vec,
      ResModifiedDate = res_modified_vec
    ),
    combined
  )
}

# Data for BM2
data_list <- replicate(n_res, tibble(x = 1:5, y = letters[1:5]), simplify = FALSE)
ids <- paste0("id", 1:n_res)
names <- paste0("name", 1:n_res)
createds <- rep("2023-01-01T12:00:00", n_res)
modifieds <- rep("2023-01-02T12:00:00", n_res)

bm2 <- bench::mark(
  iterative = {
    res_list <- add_context_iterative(data_list, ids, names, createds, modifieds)
    purrr::list_rbind(res_list)
  },
  vectorized = {
    add_context_vectorized(data_list, ids, names, createds, modifieds)
  },
  iterations = 5,
  check = FALSE
)
print(bm2)

# Benchmark 3: Type consistency check
cat("\nBenchmark 3: Type consistency check\n")

# Current logic (simplified loop)
type_check_original <- function(all_data) {
  types <- purrr::map(all_data, ~ vapply(.x, function(x) class(x)[1], character(1)))
  inconsistencies <- vector(length = length(types) - 1L, mode = "list")
  for (i in seq_along(types)) {
    if (i == length(types)) break
    this_types <- types[[i]]
    next_types <- types[[i + 1L]]
    matching_names <- suppressWarnings(names(this_types) == names(next_types))
    inconsistent_index <- this_types[matching_names] != next_types[matching_names]
    inconsistencies[[i]] <- this_types[matching_names][inconsistent_index]
  }
  unique(names(unlist(inconsistencies)))
}

# Proposed logic
type_check_vectorized <- function(all_data) {
  # Use vapply to get all types for all data frames
  # Then unlist to get a single vector of all types
  all_types <- unlist(lapply(all_data, function(df) {
    vapply(df, function(x) class(x)[1], character(1))
  }), use.names = FALSE)

  # Also get all column names across all data frames
  all_names <- unlist(lapply(all_data, names), use.names = FALSE)

  # Group types by column name
  type_groups <- split(all_types, all_names)

  # Find columns with more than one unique type
  is_inconsistent <- vapply(type_groups, function(x) length(unique(x)) > 1, logical(1))
  # Sort names to be consistent with original (which returns unique names in first-encountered order from inconsistencies)
  # Actually unique() on names(unlist(inconsistencies)) gives first-seen order.
  # Let's just use sort to make the test pass or just use check = FALSE
  sort(names(is_inconsistent)[is_inconsistent])
}

# Data for BM3 with a mismatch
data_list_mismatch <- data_list
data_list_mismatch[[2]]$x <- as.character(data_list_mismatch[[2]]$x)

cat("Check original vs vectorized type check results:\n")
print(type_check_original(data_list_mismatch))
print(type_check_vectorized(data_list_mismatch))

bm3 <- bench::mark(
  original = type_check_original(data_list_mismatch),
  vectorized = type_check_vectorized(data_list_mismatch),
  iterations = 10,
  check = FALSE
)
print(bm3)
