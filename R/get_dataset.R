#' Get Open Data resources from a dataset
#'
#' @description Downloads multiple resources from a dataset on the NHS Open Data platform by dataset name, with optional row limits and context columns.
#'
#' @param dataset_name Name of the dataset as found on \href{https://www.opendata.nhs.scot/}{NHS Open Data platform} (character).
#' @param max_resources (optional) The maximum number of resources to return (integer). If not set, all resources are returned.
#' @inheritParams get_resource
#'
#' @seealso [get_resource()] for downloading a single resource from a dataset.
#'
#' @return A [tibble][tibble::tibble-package] with the data.
#' @export
#'
#' @examples
#' get_dataset("gp-practice-populations", max_resources = 2, rows = 10)
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
  query <- list("id" = dataset_name)
  content <- try(
    phs_GET("package_show", query),
    silent = TRUE
  )

  # if content contains a 'Not Found Error'
  # throw error with suggested dataset name
  if (grepl("Not Found Error", content[1])) {
    suggest_dataset_name(dataset_name)
  }

  # define list of resource IDs to get
  all_ids <- purrr::map_chr(content$result$resources, ~ .x$id)

  n_res <- length(all_ids)
  res_index <- 1:min(n_res, max_resources)

  selection_ids <- all_ids[res_index]

  # get all resources
  all_data <- purrr::map(
    selection_ids,
    get_resource,
    rows = rows,
    row_filters = row_filters,
    col_select = col_select,
  )

  # resolve class issues
  # Robustly get first class of each column for all resources
  types_list <- lapply(all_data, function(df) {
    vapply(df, function(col) class(col)[1], character(1))
  })

  # Flatten the list of type vectors and find columns with inconsistent types
  all_types <- do.call(c, unname(types_list))
  split_types <- split(all_types, names(all_types))
  inconsistent <- vapply(split_types, function(x) length(unique(x)) > 1, logical(1))

  # Get names of columns to coerce
  to_coerce <- names(inconsistent)[inconsistent]

  if (length(to_coerce) > 0) {
    cli::cli_warn(c(
      "Due to conflicts between column types across resources,
      the following {cli::qty(to_coerce)} column{?s} ha{?s/ve} been coerced to type character:",
      "{.val {to_coerce}}"
    ))

    # Fast batch coercion using base R
    all_data <- lapply(all_data, function(df) {
      cols <- intersect(to_coerce, names(df))
      if (length(cols) > 0) {
        df[cols] <- lapply(df[cols], as.character)
      }
      df
    })
  }

  # Combine the list of resources into a single tibble
  combined <- purrr::list_rbind(all_data)

  if (include_context) {
    # Vectorized addition of 'resource context' columns after binding
    n_rows_list <- vapply(all_data, nrow, integer(1))

    # Extract context values safely
    res_list <- content$result$resources[res_index]
    res_names <- vapply(res_list, function(x) {
      if (is.null(x$name)) NA_character_ else x$name
    }, character(1))
    created_dates <- vapply(res_list, function(x) {
      if (is.null(x$created)) NA_character_ else x$created
    }, character(1))
    modified_dates <- vapply(res_list, function(x) {
      if (is.null(x$last_modified)) NA_character_ else x$last_modified
    }, character(1))

    # Parse dates once
    created_dates_posix <- as.POSIXct(created_dates, format = "%FT%X", tz = "UTC")
    modified_dates_posix <- as.POSIXct(modified_dates, format = "%FT%X", tz = "UTC")

    # The platform can record the modified date as being before the created date
    # by a few microseconds, this will catch any rounding which ensure
    # created_date is always <= modified_date
    fix_idx <- !is.na(modified_dates_posix) & !is.na(created_dates_posix) &
      modified_dates_posix < created_dates_posix
    # Ensure fix_idx is logical and doesn't contain NAs for indexing
    fix_idx[is.na(fix_idx)] <- FALSE
    modified_dates_posix[fix_idx] <- created_dates_posix[fix_idx]

    context_df <- tibble::tibble(
      "ResID" = rep(selection_ids, n_rows_list),
      "ResName" = rep(res_names, n_rows_list),
      "ResCreatedDate" = rep(created_dates_posix, n_rows_list),
      "ResModifiedDate" = rep(modified_dates_posix, n_rows_list)
    )

    # Ensure overwriting behavior
    combined <- combined[, setdiff(names(combined), names(context_df)), drop = FALSE]

    combined <- dplyr::bind_cols(context_df, combined)
  }

  return(combined)
}
