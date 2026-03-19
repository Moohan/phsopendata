library(purrr)
library(bench)

# Simulate all_data: a list of 400 data frames, each with 10 columns
n_res <- 400
n_cols <- 10
col_names <- paste0("col", 1:n_cols)

generate_df <- function() {
  as.data.frame(setNames(replicate(n_cols, runif(10), simplify = FALSE), col_names))
}

all_data <- replicate(n_res, generate_df(), simplify = FALSE)

# Introduce some inconsistencies
all_data[[100]]$col5 <- as.character(all_data[[100]]$col5)
all_data[[200]]$col2 <- as.integer(all_data[[200]]$col2)

original_type_check <- function(all_data) {
  types <- purrr::map(
    all_data,
    function(df) vapply(df, function(x) class(x)[1], character(1))
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
  unique(names(unlist(inconsistencies)))
}

vectorized_type_check <- function(all_data) {
  # Extract first class for each column in each data frame
  all_types <- unlist(lapply(all_data, function(df) {
    vapply(df, function(x) class(x)[1L], character(1L))
  }), use.names = FALSE)

  all_names <- unlist(lapply(all_data, names), use.names = FALSE)

  # Split types by column names
  types_by_col <- split(all_types, all_names)

  # Identify columns with more than one unique type
  is_inconsistent <- vapply(types_by_col, function(x) length(unique(x)) > 1L, logical(1L))

  names(is_inconsistent)[is_inconsistent]
}

# Verify correctness
res_orig <- original_type_check(all_data)
res_vec <- vectorized_type_check(all_data)
stopifnot(setequal(res_orig, res_vec))

# Benchmark
print(bench::mark(
  original = original_type_check(all_data),
  vectorized = vectorized_type_check(all_data),
  check = FALSE
))
