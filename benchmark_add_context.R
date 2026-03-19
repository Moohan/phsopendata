library(dplyr)
library(purrr)
library(bench)

# Simulate data
n_res <- 400
n_rows_per_res <- 100
all_data <- replicate(n_res, tibble(col1 = runif(n_rows_per_res), col2 = sample(letters, n_rows_per_res, replace = TRUE)), simplify = FALSE)

selection_ids <- paste0("id", 1:n_res)
names <- paste0("name", 1:n_res)
created_dates <- rep("2023-01-01T12:00:00", n_res)
modified_dates <- rep("2023-01-02T12:00:00", n_res)

# Mock add_context
add_context_mock <- function(data, id, name, created_date, modified_date) {
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

original_way <- function() {
  all_data_with_context <- purrr::pmap(
    list(
      data = all_data,
      id = selection_ids,
      name = names,
      created_date = created_dates,
      modified_date = modified_dates
    ),
    add_context_mock
  )
  purrr::list_rbind(all_data_with_context)
}

vectorized_way <- function() {
  # Record the number of rows in each data frame
  res_row_counts <- vapply(all_data, nrow, integer(1L))

  # Combine first
  combined <- purrr::list_rbind(all_data)

  # Prepare context vectors
  # Pre-parse dates to avoid redundant parsing
  p_created_dates <- as.POSIXct(created_dates, format = "%FT%X", tz = "UTC")
  p_modified_dates <- as.POSIXct(modified_dates, format = "%FT%X", tz = "UTC")

  # Handle the modified < created case
  p_modified_dates <- p_created_dates + pmax(0, difftime(p_modified_dates, p_created_dates, units = "secs"))
  # Simplified for the mock, actually we should use the same logic as add_context

  # Re-implementing add_context logic vectorially
  final_modified_dates <- p_modified_dates
  mask <- !is.na(p_modified_dates) & p_modified_dates < p_created_dates
  final_modified_dates[mask] <- p_created_dates[mask]

  # Repeat each context value for each row in its corresponding resource
  res_idx <- rep(seq_along(all_data), res_row_counts)

  combined <- combined %>%
    mutate(
      ResID = selection_ids[res_idx],
      ResName = names[res_idx],
      ResCreatedDate = p_created_dates[res_idx],
      ResModifiedDate = final_modified_dates[res_idx],
      .before = everything()
    )
  combined
}

# Verify
res1 <- original_way()
res2 <- vectorized_way()
# Date classes might have different attributes after bind_rows vs vector creation, so we check equality
stopifnot(all(res1$ResID == res2$ResID))
stopifnot(all(res1$ResName == res2$ResName))
stopifnot(all(res1$ResCreatedDate == res2$ResCreatedDate))
stopifnot(all(res1$ResModifiedDate == res2$ResModifiedDate))

print(bench::mark(
  original = original_way(),
  vectorized = vectorized_way(),
  check = FALSE
))
