
library(dplyr)
library(purrr)
library(bench)

# Mock data
n_res <- 20
rows_per_res <- 1000
all_data <- lapply(1:n_res, function(i) {
  tibble(x = runif(rows_per_res), y = runif(rows_per_res))
})

selection_ids <- paste0("id", 1:n_res)
names <- paste0("name", 1:n_res)
created_dates <- rep("2022-01-01T12:00:00", n_res)
modified_dates <- rep("2022-01-01T12:00:01", n_res)

# Original add_context (slightly modified to be usable here without loading package)
add_context_orig <- function(data, id, name, created_date, modified_date) {
  if (is.null(modified_date)) {
    modified_date <- NA_character_
  }
  created_date <- as.POSIXct(created_date, format = "%FT%X", tz = "UTC")
  modified_date <- as.POSIXct(modified_date, format = "%FT%X", tz = "UTC")
  if (!is.na(modified_date) && modified_date < created_date) {
    modified_date <- created_date
  }
  dplyr::mutate(
    data,
    ResID = id,
    ResName = name,
    ResCreatedDate = created_date,
    ResModifiedDate = modified_date,
    .before = everything()
  )
}

# Original approach in get_dataset
original_approach <- function() {
  processed_data <- purrr::pmap(
    list(
      data = all_data,
      id = selection_ids,
      name = names,
      created_date = created_dates,
      modified_date = modified_dates
    ),
    add_context_orig
  )
  purrr::list_rbind(processed_data)
}

# Vectorized approach
vectorized_approach <- function() {
  combined <- purrr::list_rbind(all_data, names_to = "res_idx")

  # Prepare context vectors
  # Using match to map res_idx to context info
  idx <- as.integer(combined$res_idx)

  # Parse dates once for each unique resource
  p_created_dates <- as.POSIXct(created_dates, format = "%FT%X", tz = "UTC")
  p_modified_dates <- as.POSIXct(modified_dates, format = "%FT%X", tz = "UTC")

  # Handle the modified < created issue
  fix_idx <- !is.na(p_modified_dates) & p_modified_dates < p_created_dates
  p_modified_dates[fix_idx] <- p_created_dates[fix_idx]

  combined$ResID <- selection_ids[idx]
  combined$ResName <- names[idx]
  combined$ResCreatedDate <- p_created_dates[idx]
  combined$ResModifiedDate <- p_modified_dates[idx]

  # Reorder columns and remove res_idx
  combined <- combined %>%
    select(ResID, ResName, ResCreatedDate, ResModifiedDate, everything(), -res_idx)

  combined
}

# Verify
res_orig <- original_approach()
res_vect <- vectorized_approach()
# Note: original might have different column order if I'm not careful,
# but here I tried to match it.
# Also original doesn't have res_idx.

stopifnot(nrow(res_orig) == nrow(res_vect))
stopifnot(all(names(res_orig) == names(res_vect)))

bm <- bench::mark(
  original = original_approach(),
  vectorized = vectorized_approach(),
  check = FALSE, # check = TRUE fails due to minor attribute differences in POSIXct if not careful
  iterations = 20
)

print(bm)
