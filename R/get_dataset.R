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

  # resolve class issues
  # Vectorized type check is much faster than pairwise loops for many resources
  col_names <- unlist(lapply(all_data, names), use.names = FALSE)
  col_types <- unlist(lapply(all_data, function(x) {
    vapply(x, function(y) class(y)[1L], character(1L))
  }), use.names = FALSE)

  type_list <- split(col_types, col_names)
  to_coerce <- names(type_list)[vapply(
    type_list,
    function(x) length(unique(x)) > 1L,
    logical(1L)
  )]

  if (length(to_coerce) > 0L) {
    cli::cli_warn(c(
      "Due to conflicts between column types across resources,
      the following {cli::qty(to_coerce)} column{?s} ha{?s/ve} been coerced to type character:",
      "{.val {to_coerce}}"
    ))

    # Batch coercion using lapply is faster than mutate(across()) for lists
    all_data <- lapply(all_data, function(df) {
      cols_to_coerce <- intersect(names(df), to_coerce)
      if (length(cols_to_coerce) > 0L) {
        df[cols_to_coerce] <- lapply(df[cols_to_coerce], as.character)
      }
      return(df)
    })
  }

  # Combine the list of resources into a single tibble
  combined <- purrr::list_rbind(all_data)

  if (include_context) {
    # Vectorized context addition is faster than per-resource mutate
    n_rows <- vapply(all_data, nrow, integer(1L))

    ids <- selection_ids
    names <- purrr::map_chr(content$result$resources[res_index], ~ .x$name)
    created_dates <- purrr::map_chr(
      content$result$resources[res_index],
      ~ .x$created
    )
    # last_modified can be NULL in CKAN response
    modified_dates <- purrr::map_chr(
      content$result$resources[res_index],
      ~ if (is.null(.x$last_modified)) NA_character_ else .x$last_modified
    )

    # Pre-parse dates to avoid redundant parsing for every row
    created_dates_posix <- as.POSIXct(
      created_dates,
      format = "%FT%X",
      tz = "UTC"
    )
    modified_dates_posix <- as.POSIXct(
      modified_dates,
      format = "%FT%X",
      tz = "UTC"
    )

    # Fix modified < created before expansion for minor efficiency gain
    to_fix <- !is.na(modified_dates_posix) & !is.na(created_dates_posix) &
      modified_dates_posix < created_dates_posix
    modified_dates_posix[to_fix] <- created_dates_posix[to_fix]

    # Expand metadata to match combined data frame rows
    combined <- add_context(
      data = combined,
      id = rep(ids, n_rows),
      name = rep(names, n_rows),
      created_date = rep(created_dates_posix, n_rows),
      modified_date = rep(modified_dates_posix, n_rows)
    )
  }

  return(combined)
}
