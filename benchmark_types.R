
library(bench)
library(purrr)

# Mock data
n_res <- 400
n_cols <- 20
cols <- paste0("col", 1:n_cols)
all_data <- replicate(n_res, {
  df <- as.data.frame(matrix(runif(10 * n_cols), nrow = 10))
  names(df) <- cols
  # Introduce some type inconsistencies
  if (runif(1) > 0.95) {
    df[[sample(cols, 1)]] <- as.character(df[[sample(cols, 1)]])
  }
  df
}, simplify = FALSE)

# Old way
check_types_old <- function(all_data) {
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
  return(to_coerce)
}

# New vectorized way
check_types_new <- function(all_data) {
  # Extract first class of each column for all dataframes
  all_types <- unlist(lapply(all_data, function(df) vapply(df, function(x) class(x)[1], character(1))), use.names = FALSE)
  all_names <- unlist(lapply(all_data, names), use.names = FALSE)

  # Group types by column name
  type_groups <- split(all_types, all_names)

  # Find columns with more than one unique type
  to_coerce <- names(type_groups)[vapply(type_groups, function(x) length(unique(x)) > 1, logical(1))]

  return(to_coerce)
}

# Verification
old_res <- check_types_old(all_data)
new_res <- check_types_new(all_data)
stopifnot(setequal(old_res, new_res))

# Benchmarking
results <- bench::mark(
  old = check_types_old(all_data),
  new = check_types_new(all_data),
  check = FALSE # setequal used above
)

print(results)
