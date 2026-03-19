library(dplyr)
library(purrr)
library(bench)

# Mock some dependencies or load package
.libPaths('~/.R/library')
pkgload::load_all()

# We need to mock get_dataset's internal calls since we cannot easily mock exported functions that call each other in the same package during load_all()
# Actually, let's redefine get_dataset for verification if needed, but I want to test the actual package code.

# Let's mock at the lowest possible level: phs_GET and get_resource
# Tracing should work even for internal calls if we do it right.

# Define the mock functions
mock_get_resource <- function(res_id, rows = NULL, row_filters = NULL, col_select = NULL, include_context = FALSE) {
  n_rows <- 100
  tibble::tibble(col1 = runif(n_rows), col2 = sample(letters, n_rows, replace = TRUE))
}

mock_phs_GET <- function(action, query = list()) {
  if (action == "package_show") {
    n_res <- 400
    resources <- lapply(1:n_res, function(i) {
      list(
        id = paste0("id", i),
        name = paste0("name", i),
        created = "2023-01-01T12:00:00",
        last_modified = "2023-01-02T12:00:00"
      )
    })
    return(list(result = list(resources = resources)))
  }
  # Fallback
  list()
}

# Inject mocks
assignInNamespace("get_resource", mock_get_resource, ns = "phsopendata")
assignInNamespace("phs_GET", mock_phs_GET, ns = "phsopendata")
assignInNamespace("check_dataset_name", function(x) NULL, ns = "phsopendata")

# Test get_dataset
t1 <- system.time(res <- get_dataset("test-dataset", include_context = TRUE))
print(t1)
print(dim(res))

# Verification of context
stopifnot(nrow(res) == 400 * 100)
stopifnot(all(c("ResID", "ResName", "ResCreatedDate", "ResModifiedDate") %in% names(res)))
print("Verification successful!")

# Compare with old way (simulated by manual pmap/list_rbind)
old_way_time <- system.time({
  content <- mock_phs_GET("package_show")
  selection_ids <- map_chr(content$result$resources, ~.x$id)
  all_data <- map(selection_ids, mock_get_resource)
  # Simple mock of add_context as it was
  res_index <- 1:400
  all_data_with_context <- pmap(
    list(
      data = all_data,
      id = selection_ids,
      name = map_chr(content$result$resources[res_index], ~ .x$name),
      created_date = map_chr(content$result$resources[res_index], ~ .x$created),
      modified_date = map_chr(content$result$resources[res_index], ~ .x$last_modified)
    ),
    phsopendata:::add_context
  )
  combined_old <- list_rbind(all_data_with_context)
})
print("Old way (simulated) time:")
print(old_way_time)
