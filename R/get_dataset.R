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
  res_index <- seq_len(min(n_res, if (is.null(max_resources)) n_res else max_resources))

  selection_ids <- all_ids[res_index]

  # get all resources
  all_data <- purrr::map(
    selection_ids,
    get_resource,
    rows = rows,
    row_filters = row_filters,
    col_select = col_select
  )

  # resolve class issues. Vectorized approach for performance.
  all_names <- unlist(lapply(all_data, names), use.names = FALSE)
  all_types <- unlist(lapply(all_data, function(df) {
    vapply(df, function(x) class(x)[1L], character(1L))
  }), use.names = FALSE)

  if (length(all_names) > 0L) {
    type_groups <- split(all_types, all_names)
    to_coerce <- names(type_groups)[vapply(type_groups, function(x) {
      any(x != x[1L])
    }, logical(1L))]
  } else {
    to_coerce <- character(0)
  }

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

  if (include_context && nrow(combined) > 0L) {
    # Add the 'resource context' as columns to the data.
    # Vectorized context addition after combining is significantly faster.
    n_rows <- vapply(all_data, nrow, integer(1L))
    res_names <- purrr::map_chr(content$result$resources[res_index], ~ .x$name)
    created_dates <- purrr::map_chr(
      content$result$resources[res_index],
      ~ .x$created
    )
    modified_dates <- vapply(
      content$result$resources[res_index],
      function(x) if (is.null(x$last_modified)) NA_character_ else x$last_modified,
      character(1L)
    )

    combined <- add_context(
      data = combined,
      id = rep(selection_ids, n_rows),
      name = rep(res_names, n_rows),
      created_date = rep(created_dates, n_rows),
      modified_date = rep(modified_dates, n_rows)
    )
  } else if (include_context) {
    # Handle the case where combined is empty but context is requested
    combined <- tibble::add_column(
      combined,
      ResID = character(0),
      ResName = character(0),
      ResCreatedDate = as.POSIXct(character(0)),
      ResModifiedDate = as.POSIXct(character(0)),
      .before = 1L
    )
  }

  return(combined)
}
