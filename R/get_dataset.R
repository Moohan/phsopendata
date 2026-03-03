#' Get Open Data resources from a dataset
#'
#' @description Downloads multiple resources from a dataset on the NHS Open Data
#'  platform by dataset name, with optional row limits and context columns.
#'
#' @param dataset_name Name of the dataset as found on the
#' [NHS Open Data platform](https://www.opendata.nhs.scot) (character).
#' @param max_resources (optional) The maximum number of resources to return
#' (integer). If not set, all resources are returned.
#' @inheritParams get_resource
#'
#' @seealso [get_resource()] for downloading a single resource from a dataset.
#'
#' @return A [tibble][tibble::tibble-package] with the data.
#' @export
#'
#' @examplesIf isTRUE(length(curl::nslookup("www.opendata.nhs.scot", error = FALSE)) > 0L)
#' \dontrun{
#' get_dataset("gp-practice-populations", max_resources = 2, rows = 10)
#' }
get_dataset <- function(
  dataset_name,
  max_resources = NULL,
  rows = NULL,
  row_filters = NULL,
  col_select = NULL,
  include_context = FALSE
) {
  # throw error if name type/format is invalid
  check_dataset_name(dataset_name)

  # define query and try API call
  query <- list(id = dataset_name)
  content <- try(
    phs_GET("package_show", query),
    silent = TRUE
  )

  # if content contains a 'Not Found Error'
  # throw error with suggested dataset name
  if (grepl("Not Found Error", content[1L], fixed = TRUE)) {
    suggest_dataset_name(dataset_name)
  }

  # define list of resource IDs to get
  all_ids <- purrr::map_chr(content$result$resources, ~ .x$id)

  n_res <- length(all_ids)
  res_index <- 1L:min(n_res, max_resources)

  selection_ids <- all_ids[res_index]

  # get all resources
  all_data <- purrr::map(
    selection_ids,
    get_resource,
    rows = rows,
    row_filters = row_filters,
    col_select = col_select
  )

  # Optimized type inconsistency check
  # Flatten all column names and types into single vectors
  all_names <- unlist(lapply(all_data, names), use.names = FALSE)
  all_types <- unlist(lapply(all_data, function(df) {
    vapply(df, function(x) class(x)[1], character(1))
  }), use.names = FALSE)

  # Group types by column name and find those with > 1 unique type
  type_splits <- split(all_types, all_names)
  to_coerce <- names(type_splits)[vapply(type_splits, function(x) {
    length(unique(x)) > 1
  }, logical(1))]

  if (length(to_coerce) > 0L) {
    cli::cli_warn(c(
      "Due to conflicts between column types across resources,
      the following {cli::qty(to_coerce)} column{?s} ha{?s/ve} been coerced to type character:",
      "{.val {to_coerce}}"
    ))

    # Use base R for character coercion as it's faster than dplyr::across
    all_data <- lapply(all_data, function(df) {
      cols_to_fix <- intersect(names(df), to_coerce)
      if (length(cols_to_fix) > 0) {
        for (col in cols_to_fix) {
          df[[col]] <- as.character(df[[col]])
        }
      }
      return(df)
    })
  }

  # Combine the list of resources into a single tibble
  combined <- purrr::list_rbind(all_data)

  if (include_context && nrow(combined) > 0) {
    # Vectorized context addition
    # Pre-calculate counts and indices for metadata expansion
    n_rows <- vapply(all_data, nrow, integer(1))
    res_idx <- rep(seq_along(all_data), n_rows)

    # Extract and expand metadata
    ids <- selection_ids[res_idx]
    names <- purrr::map_chr(content$result$resources[res_index], ~ .x$name)[res_idx]
    created_dates <- purrr::map_chr(
      content$result$resources[res_index],
      ~ .x$created
    )[res_idx]
    modified_dates <- purrr::map_chr(
      content$result$resources[res_index],
      ~ if (is.null(.x$last_modified)) NA_character_ else .x$last_modified
    )[res_idx]

    # Pre-parse dates once before adding context to avoid redundant parsing
    # in add_context if it were called iteratively.
    created_dates <- as.POSIXct(created_dates, format = "%FT%X", tz = "UTC")
    modified_dates <- as.POSIXct(modified_dates, format = "%FT%X", tz = "UTC")

    combined <- add_context(
      data = combined,
      id = ids,
      name = names,
      created_date = created_dates,
      modified_date = modified_dates
    )
  }

  return(combined)
}
