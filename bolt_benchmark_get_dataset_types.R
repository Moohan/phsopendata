
library(dplyr)
library(purrr)
library(bench)

# Simulate all_data with 10 data frames, each having 20 columns
n_df <- 10
n_col <- 20
all_data <- lapply(1:n_df, function(i) {
  df <- as.data.frame(matrix(runif(100 * n_col), nrow = 100))
  colnames(df) <- paste0("col", 1:n_col)
  # Introduce some POSIXct
  df$col1 <- Sys.time()
  # Introduce some inconsistencies
  if (i %% 2 == 0) {
    df$col2 <- as.character(df$col2)
  }
  df
})

# Current implementation
current_impl <- function(all_data) {
  # This part is prone to error if class() returns > 1 element
  # but here we use data frames so it might be okay for some types
  # Actually POSIXct returns c("POSIXct", "POSIXt")
  types <- purrr::map(
    all_data,
    function(df) {
        # Using map_chr here will fail if any col has > 1 class
        vapply(df, function(x) class(x)[1], character(1))
    }
  )

  # for each df, check if next df class matches
  inconsistencies <- vector(length = length(types) - 1L, mode = "list")
  for (i in seq_along(types)) {
    if (i == length(types)) break

    this_types <- types[[i]]
    next_types <- types[[i + 1L]]

    # find matching names
    matching_names <- suppressWarnings(
      names(this_types) == names(next_types)
    )

    # of matching name cols, find if types match too
    inconsistent_index <- this_types[matching_names] !=
      next_types[matching_names]
    inconsistencies[[i]] <- this_types[matching_names][inconsistent_index]
  }

  # define which columns to coerce and warn
  to_coerce <- unique(names(unlist(inconsistencies)))
  return(to_coerce)
}

# Optimized implementation
optimized_impl <- function(all_data) {
  all_types <- unlist(lapply(all_data, function(df) {
    vapply(df, function(x) class(x)[1L], character(1L))
  }), use.names = FALSE)

  all_names <- unlist(lapply(all_data, names), use.names = FALSE)

  split_types <- split(all_types, all_names)

  inconsistent <- vapply(split_types, function(x) length(unique(x)) > 1L, logical(1L))

  to_coerce <- names(inconsistent)[inconsistent]
  return(to_coerce)
}

# Verify correctness
stopifnot(identical(sort(current_impl(all_data)), sort(optimized_impl(all_data))))

bm <- bench::mark(
  current = current_impl(all_data),
  optimized = optimized_impl(all_data),
  iterations = 100
)

print(bm)
