
library(bench)
library(dplyr)

# Mock data and functions
add_context_old <- function(data, id, name, created_date, modified_date) {
  if (is.null(modified_date)) {
    modified_date <- NA_character_
  }
  created_date <- as.POSIXct(created_date, format = "%FT%X", tz = "UTC")
  modified_date <- as.POSIXct(modified_date, format = "%FT%X", tz = "UTC")
  if (!is.na(modified_date) && modified_date < created_date) {
    modified_date <- created_date
  }
  data_with_context <- dplyr::mutate(
    data,
    ResID = id,
    ResName = name,
    ResCreatedDate = created_date,
    ResModifiedDate = modified_date,
    .before = dplyr::everything()
  )
  return(data_with_context)
}

add_context_new <- function(data, id, name, created_date, modified_date) {
  if (is.null(modified_date)) {
    modified_date <- NA_character_
  }
  # Pre-parse if they are characters, but they might already be POSIXct if passed from list_resources or similar logic
  # In get_dataset, they are passed as characters from purrr::map_chr
  if (is.character(created_date)) created_date <- as.POSIXct(created_date, format = "%FT%X", tz = "UTC")
  if (is.character(modified_date)) modified_date <- as.POSIXct(modified_date, format = "%FT%X", tz = "UTC")

  if (!is.na(modified_date) && modified_date < created_date) {
    modified_date <- created_date
  }

  # Fast assignment for common case
  data$ResID <- id
  data$ResName <- name
  data$ResCreatedDate <- created_date
  data$ResModifiedDate <- modified_date

  # Reorder columns to put context first
  nc <- ncol(data)
  data <- data[, c((nc-3):nc, 1:(nc-4)), drop = FALSE]

  return(data)
}

# Benchmarking
n_rows <- 10000
test_data <- data.frame(a = runif(n_rows), b = runif(n_rows), c = runif(n_rows))
res_id <- "some-id"
res_name <- "some-name"
created <- "2023-01-01T12:00:00"
modified <- "2023-01-02T12:00:00"

results <- bench::mark(
  old = add_context_old(test_data, res_id, res_name, created, modified),
  new = add_context_new(test_data, res_id, res_name, created, modified),
  check = TRUE
)

print(results)
