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
  all_ids <- vapply(
    content$result$resources,
    function(x) if (is.null(x$id)) NA_character_ else x$id,
    character(1L)
  )

  n_res <- length(all_ids)
  res_index <- seq_len(min(n_res, max_resources))

  selection_ids <- all_ids[res_index]

  # get all resources
  all_data <- purrr::map(
    selection_ids,
    get_resource,
    rows = rows,
    row_filters = row_filters,
    col_select = col_select
  )

  # resolve class issues
  # Use vapply for speed and to handle multi-class objects (e.g. POSIXct)
  types_list <- lapply(all_data, function(df) {
    vapply(df, function(col) class(col)[1L], character(1L))
  })

  # Flatten the list of type vectors and group by column name to find inconsistencies
  all_types <- do.call(c, unname(types_list))
  grouped_types <- split(all_types, names(all_types))
  is_inconsistent <- vapply(
    grouped_types,
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

    # Batch coercion using base R is significantly faster than dplyr::mutate
    all_data <- lapply(all_data, function(df) {
      cols_to_fix <- intersect(to_coerce, names(df))
      if (length(cols_to_fix) > 0L) {
        df[cols_to_fix] <- lapply(df[cols_to_fix], as.character)
      }
      df
    })
  }

  # Combine the list of resources into a single tibble
  combined <- purrr::list_rbind(all_data)

  if (include_context) {
    # Add the 'resource context' as columns to the data in a vectorized way
    res_lengths <- vapply(all_data, nrow, integer(1L))

    created_dates <- vapply(
      content$result$resources[res_index],
      function(x) if (is.null(x$created)) NA_character_ else x$created,
      character(1L)
    )
    modified_dates <- vapply(
      content$result$resources[res_index],
      function(x) {
        if (is.null(x$last_modified)) NA_character_ else x$last_modified
      },
      character(1L)
    )

    # Parse and process dates using a robust format string
    created_dates_posix <- as.POSIXct(
      created_dates,
      format = "%Y-%m-%dT%H:%M:%S",
      tz = "UTC"
    )
    modified_dates_posix <- as.POSIXct(
      modified_dates,
      format = "%Y-%m-%dT%H:%M:%S",
      tz = "UTC"
    )

    # Ensure created_date <= modified_date, safely handling NAs
    idx_swap <- !is.na(modified_dates_posix) &
      !is.na(created_dates_posix) &
      modified_dates_posix < created_dates_posix
    modified_dates_posix[idx_swap] <- created_dates_posix[idx_swap]

    res_names <- vapply(
      content$result$resources[res_index],
      function(x) if (is.null(x$name)) NA_character_ else x$name,
      character(1L)
    )

    context_df <- tibble::tibble(
      ResID = rep(selection_ids, res_lengths),
      ResName = rep(res_names, res_lengths),
      ResCreatedDate = rep(created_dates_posix, res_lengths),
      ResModifiedDate = rep(modified_dates_posix, res_lengths)
    )

    # Prepend context columns to the combined data
    combined <- dplyr::bind_cols(context_df, combined)
  }

  return(combined)
}
