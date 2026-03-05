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

  # resolve class issues across resources efficiently
  # Extract all column names and their first class
  all_cols <- unlist(lapply(all_data, names), use.names = FALSE)
  all_types <- unlist(lapply(all_data, function(df) {
    vapply(df, function(x) class(x)[1L], character(1L))
  }), use.names = FALSE)

  # Group types by column name and find those with more than one unique type
  type_splits <- split(all_types, all_cols)
  to_coerce <- names(type_splits)[vapply(type_splits, function(x) {
    length(unique(x)) > 1L
  }, logical(1L))]

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
    # Add the 'resource context' as columns to the data in a vectorized way
    # Pre-parse dates to POSIXct to avoid repeated parsing overhead in add_context
    res_info <- content$result$resources[res_index]

    all_res_ids <- selection_ids
    all_res_names <- purrr::map_chr(
      res_info,
      function(x) if (is.null(x$name)) NA_character_ else x$name
    )
    all_res_created <- as.POSIXct(
      purrr::map_chr(
        res_info,
        function(x) if (is.null(x$created)) NA_character_ else x$created
      ),
      format = "%FT%X",
      tz = "UTC"
    )
    all_res_modified <- as.POSIXct(
      purrr::map_chr(
        res_info,
        function(x) if (is.null(x$last_modified)) NA_character_ else x$last_modified
      ),
      format = "%FT%X",
      tz = "UTC"
    )

    # Map the resource-level metadata to the row-level combined data frame
    # Create an index based on the number of rows in each resource
    idx <- rep(seq_along(all_data), vapply(all_data, nrow, integer(1L)))

    combined <- add_context(
      data = combined,
      id = all_res_ids[idx],
      name = all_res_names[idx],
      created_date = all_res_created[idx],
      modified_date = all_res_modified[idx]
    )
  }

  return(combined)
}
