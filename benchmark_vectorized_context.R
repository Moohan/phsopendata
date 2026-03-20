
library(bench)
library(dplyr)
library(purrr)

# Mock data
n_res <- 400
n_rows <- 100
all_data <- replicate(n_res, {
  data.frame(a = runif(n_rows), b = runif(n_rows))
}, simplify = FALSE)

res_ids <- paste0("id-", 1:n_res)
res_names <- paste0("name-", 1:n_res)
res_created <- rep("2023-01-01T12:00:00", n_res)
res_modified <- rep("2023-01-02T12:00:00", n_res)

# Mock add_context
add_context <- function(data, id, name, created_date, modified_date) {
  if (is.null(modified_date)) modified_date <- NA_character_
  created_date <- as.POSIXct(created_date, format = "%FT%X", tz = "UTC")
  modified_date <- as.POSIXct(modified_date, format = "%FT%X", tz = "UTC")
  if (!is.na(modified_date) && modified_date < created_date) modified_date <- created_date
  dplyr::mutate(data, ResID = id, ResName = name, ResCreatedDate = created_date, ResModifiedDate = modified_date, .before = dplyr::everything())
}

# Old way
old_way <- function(all_data, res_ids, res_names, res_created, res_modified) {
  purrr::pmap(
    list(
      data = all_data,
      id = res_ids,
      name = res_names,
      created_date = res_created,
      modified_date = res_modified
    ),
    add_context
  ) %>% purrr::list_rbind()
}

# Vectorized way
new_way <- function(all_data, res_ids, res_names, res_created, res_modified) {
  combined <- purrr::list_rbind(all_data, names_to = "res_idx")
  idx <- as.integer(combined$res_idx)

  # Pre-parse dates to POSIXct once
  p_created <- as.POSIXct(res_created, format = "%FT%X", tz = "UTC")
  p_modified <- as.POSIXct(res_modified, format = "%FT%X", tz = "UTC")
  p_modified <- p_modified %||% rep(as.POSIXct(NA), length(p_created))
  # modified < created check
  fix_idx <- !is.na(p_modified) & p_modified < p_created
  p_modified[fix_idx] <- p_created[fix_idx]

  combined$ResID <- res_ids[idx]
  combined$ResName <- res_names[idx]
  combined$ResCreatedDate <- p_created[idx]
  combined$ResModifiedDate <- p_modified[idx]

  # Reorder
  nc <- ncol(combined)
  combined <- combined[, c((nc-3):nc, 2:(nc-4)), drop = FALSE]
  return(combined)
}

`%||%` <- function(x, y) if (is.null(x)) y else x

# Benchmarking
results <- bench::mark(
  old = old_way(all_data, res_ids, res_names, res_created, res_modified),
  new = new_way(all_data, res_ids, res_names, res_created, res_modified),
  check = FALSE
)

print(results)
