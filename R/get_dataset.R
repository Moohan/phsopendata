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

  # Identify columns with inconsistent types across resources
  # This vectorized approach is faster and handles multi-class columns robustly
  all_names <- unlist(lapply(all_data, names), use.names = FALSE)
  all_types <- unlist(lapply(all_data, function(x) {
    vapply(x, function(y) class(y)[1L], character(1L))
  }), use.names = FALSE)

  type_splits <- split(all_types, all_names)
  is_inconsistent <- vapply(
    type_splits,
    function(x) length(unique(x)) > 1L,
    logical(1L)
  )
  to_coerce <- names(is_inconsistent)[is_inconsistent]

  if (length(to_coerce) > 0L) {
    cli::cli_warn(c(
      "Due to conflicts between column types across resources,
      the following {cli::qty(to_coerce)} column{?s} ha{?s/ve} been coerced to type character:",
      "{.val {to_coerce}}"
    ))

    all_data <- purrr::map(
      all_data,
      dplyr::mutate,
      dplyr::across(
        dplyr::any_of(to_coerce),
        as.character
      )
    )
  }

  # Combine the list of resources into a single tibble
  combined <- purrr::list_rbind(all_data)

  if (include_context) {
    # Extract metadata for selected resources
    res_metadata <- content$result$resources[res_index]

    # Pre-parse metadata to avoid redundant parsing for every row
    # Vectorizing this here provides a significant performance boost
    res_ids <- selection_ids
    res_names <- purrr::map_chr(res_metadata, ~ .x$name)
    res_created <- purrr::map_chr(res_metadata, ~ .x$created)
    res_modified <- purrr::map_chr(
      res_metadata,
      ~ if (is.null(.x$last_modified)) NA_character_ else .x$last_modified
    )

    # Generate an index to map each row of 'combined' to its original resource
    res_lengths <- vapply(all_data, nrow, integer(1L))
    row_idx <- rep(seq_along(all_data), res_lengths)

    # Add context in a fully vectorized manner
    combined <- add_context(
      data = combined,
      id = res_ids[row_idx],
      name = res_names[row_idx],
      created_date = res_created[row_idx],
      modified_date = res_modified[row_idx]
    )
  }

  return(combined)
}
