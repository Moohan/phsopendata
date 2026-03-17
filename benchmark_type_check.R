
# library(phsopendata)
library(bench)

# We'll use a real dataset but limit resources and rows to keep it fast but representative
dataset <- "gp-practice-populations"
max_res <- 5
rows_per_res <- 100

cat("Benchmarking get_dataset with", max_res, "resources and", rows_per_res, "rows each...\n")

# To avoid network noise, we might want to run it once to warm up or just accept it.
# Actually, the bottleneck I'm looking at (type checking) happens AFTER data is fetched.
# So I'll fetch the data once and then benchmark the processing part if possible.
# But get_dataset is one big function.

# Let's try to isolate the type checking logic for a more precise benchmark of that specific part.

all_data <- lapply(1:max_res, function(i) {
  df <- data.frame(
    a = 1:rows_per_res,
    b = as.character(1:rows_per_res),
    c = Sys.Date() + 1:rows_per_res,
    stringsAsFactors = FALSE
  )
  if (i == max_res) {
    df$a <- as.character(df$a) # Introduce inconsistency
  }
  df
})

bench_type_check_original <- function(all_data) {
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

  to_coerce <- unique(names(unlist(inconsistencies)))
  return(to_coerce)
}

bench_type_check_vectorized <- function(all_data) {
  # Extract all names and all types
  all_names <- unlist(lapply(all_data, names), use.names = FALSE)
  all_types <- unlist(lapply(all_data, function(df) {
    vapply(df, function(x) class(x)[1], character(1))
  }), use.names = FALSE)

  # Split types by names
  types_by_col <- split(all_types, all_names)

  # Identify columns with more than one unique type
  is_inconsistent <- vapply(types_by_col, function(x) length(unique(x)) > 1, logical(1))
  to_coerce <- names(types_by_col)[is_inconsistent]

  return(to_coerce)
}

# Verify they give same result
res_orig <- bench_type_check_original(all_data)
res_vect <- bench_type_check_vectorized(all_data)
stopifnot(all(res_orig == res_vect))

# Benchmark with more resources to see the difference
large_all_data <- rep(all_data, 20) # 100 resources

bm <- bench::mark(
  original = bench_type_check_original(large_all_data),
  vectorized = bench_type_check_vectorized(large_all_data),
  check = TRUE,
  iterations = 100
)

print(bm)
