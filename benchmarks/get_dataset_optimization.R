library(phsopendata)
library(dplyr)
library(purrr)
library(bench)

# Mock data
create_mock_data <- function(n_res = 10, n_rows = 100) {
  lapply(1:n_res, function(i) {
    df <- data.frame(
      id = 1:n_rows,
      value = if (i %% 2 == 0) as.character(1:n_rows) else 1:n_rows,
      constant = "constant",
      stringsAsFactors = FALSE
    )
    if (i %% 3 == 0) {
      # change column order
      df <- df[, c("value", "id", "constant")]
    }
    df
  })
}

all_data <- create_mock_data(50, 100)

# Current implementation of type resolution (extracted from get_dataset.R)
current_type_resolution <- function(all_data) {
  types <- purrr::map(
    all_data,
    purrr::map_chr,
    class
  )

  inconsistencies <- vector(length = length(types) - 1L, mode = "list")
  for (i in seq_along(types)) {
    if (i == length(types)) break

    this_types <- types[[i]]
    next_types <- types[[i + 1L]]

    matching_names <- suppressWarnings(
      names(this_types) == names(next_types)
    )

    inconsistent_index <- this_types[matching_names] !=
      next_types[matching_names]
    inconsistencies[[i]] <- this_types[matching_names][inconsistent_index]
  }

  to_coerce <- unique(names(unlist(inconsistencies)))

  if (length(to_coerce) > 0L) {
    all_data <- purrr::map(
      all_data,
      dplyr::mutate,
      dplyr::across(
        dplyr::any_of(to_coerce),
        as.character
      )
    )
  }
  return(all_data)
}

# Optimized implementation
optimized_type_resolution <- function(all_data) {
  # 1. Use vapply for faster and safer class extraction
  # 2. Extract only first class to handle multi-class objects
  types_list <- lapply(all_data, function(df) {
    vapply(df, function(x) class(x)[1L], character(1L))
  })

  # 3. Flatten and find inconsistencies across ALL data frames, not just consecutive ones
  # 4. Correctly handle differing column orders
  all_types <- do.call(c, unname(types_list))
  all_names <- names(all_types)

  # Split types by name and find those with > 1 unique type
  type_counts <- split(all_types, all_names)
  to_coerce <- names(type_counts)[vapply(type_counts, function(x) length(unique(x)) > 1L, logical(1L))]

  if (length(to_coerce) > 0L) {
    # 5. Use base R for faster batch coercion
    all_data <- lapply(all_data, function(df) {
      cols_present <- intersect(to_coerce, names(df))
      if (length(cols_present) > 0) {
        for (col in cols_present) {
          df[[col]] <- as.character(df[[col]])
        }
      }
      df
    })
  }
  return(all_data)
}

# Verify correctness with differing column orders
df1 <- data.frame(A = 1, B = 2)
df2 <- data.frame(B = "2", A = "1")
test_data <- list(df1, df2)

cat("Current detects:", paste(names(unlist(current_type_resolution(test_data))), collapse=", "), "\n")
# Actually current_type_resolution returns all_data, let s fix it to return to_coerce for testing
get_to_coerce_current <- function(all_data) {
  types <- purrr::map(all_data, purrr::map_chr, class)
  inconsistencies <- vector(length = length(types) - 1L, mode = "list")
  for (i in seq_along(types)) {
    if (i == length(types)) break
    this_types <- types[[i]]; next_types <- types[[i + 1L]]
    matching_names <- suppressWarnings(names(this_types) == names(next_types))
    inconsistent_index <- this_types[matching_names] != next_types[matching_names]
    inconsistencies[[i]] <- this_types[matching_names][inconsistent_index]
  }
  unique(names(unlist(inconsistencies)))
}

get_to_coerce_opt <- function(all_data) {
  types_list <- lapply(all_data, function(df) vapply(df, function(x) class(x)[1L], character(1L)))
  all_types <- do.call(c, unname(types_list))
  type_counts <- split(all_types, names(all_types))
  names(type_counts)[vapply(type_counts, function(x) length(unique(x)) > 1L, logical(1L))]
}

cat("Current to_coerce for mismatched order:", paste(get_to_coerce_current(test_data), collapse=", "), "\n")
cat("Optimized to_coerce for mismatched order:", paste(get_to_coerce_opt(test_data), collapse=", "), "\n")

# Benchmark
res <- bench::mark(
  current = current_type_resolution(all_data),
  optimized = optimized_type_resolution(all_data),
  iterations = 10
)
print(res)
