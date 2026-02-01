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

  # If an error occurred in phs_GET, handle it.
  if (inherits(content, "try-error")) {
    # if content contains a 'Not Found Error'
    # throw error with suggested dataset name
    if (grepl("Not Found Error", content[1])) {
      suggest_dataset_name(dataset_name)
    }
    # Otherwise re-throw the original error
    stop(content)
  }

  # define list of resource IDs to get
  all_ids <- purrr::map_chr(content$result$resources, ~ .x$id)

  n_res <- length(all_ids)
  # Ensure res_index is a valid sequence even if n_res is 0
  res_limit <- if (is.null(max_resources)) n_res else min(n_res, max_resources)
  res_index <- seq_len(res_limit)

  selection_ids <- all_ids[res_index]

  # get all resources
  all_data <- purrr::map(
    selection_ids,
    get_resource,
    rows = rows,
    row_filters = row_filters,
    col_select = col_select,
  )

  # resolve class issues - use first class only to avoid issues with multi-class columns (e.g. POSIXct)
  types <- lapply(
    all_data,
    function(df) vapply(df, function(col) class(col)[1], character(1))
  )

  # Check for columns that have multiple types across all resources
  # Flatten the list of type vectors, preserving the column names
  all_types <- do.call(c, unname(types))

  if (length(all_types) > 0) {
    # Group types by column name
    split_types <- split(all_types, names(all_types))

    # Identify columns with more than one unique type
    to_coerce <- names(split_types)[vapply(split_types, function(x) length(unique(x)) > 1, logical(1))]

    if (length(to_coerce) > 0) {
      cli::cli_warn(c(
        "Due to conflicts between column types across resources,
        the following {cli::qty(to_coerce)} column{?s} ha{?s/ve} been coerced to type character:",
        "{.val {to_coerce}}"
      ))

      # Coerce columns to character efficiently using base R
      all_data <- lapply(all_data, function(df) {
        cols_to_fix <- intersect(names(df), to_coerce)
        if (length(cols_to_fix) > 0) {
          df[cols_to_fix] <- lapply(df[cols_to_fix], as.character)
        }
        df
      })
    }
  }

  if (include_context) {
    # Add the 'resource context' as columns to the data in a vectorized way after binding
    resources_subset <- content$result$resources[res_index]

    # Efficiently extract context info
    ids <- vapply(resources_subset, function(x) rlang::`%||%`(x$id, NA_character_), character(1))
    resource_names <- vapply(resources_subset, function(x) rlang::`%||%`(x$name, NA_character_), character(1))
    created_dates_raw <- vapply(resources_subset, function(x) rlang::`%||%`(x$created, NA_character_), character(1))
    modified_dates_raw <- vapply(resources_subset, function(x) rlang::`%||%`(x$last_modified, NA_character_), character(1))

    # Parse dates in a vectorized way using ISO8601 format
    c_dates <- as.POSIXct(created_dates_raw, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
    m_dates <- as.POSIXct(modified_dates_raw, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")

    # Adjust modified date if it's before created date
    bad_m <- !is.na(m_dates) & !is.na(c_dates) & m_dates < c_dates
    m_dates[bad_m] <- c_dates[bad_m]

    # Bind all data first, adding an index to join context info
    names(all_data) <- seq_along(all_data)
    combined <- purrr::list_rbind(all_data, names_to = "res_idx")

    # Prepend context columns efficiently.
    # We use indexing with res_idx to correctly map metadata to rows.
    # If combined is empty, res_idx_vec is integer(0), leading to empty context columns.
    res_idx_vec <- as.integer(combined[["res_idx"]])

    context_info <- tibble::tibble(
      "ResID" = ids[res_idx_vec],
      "ResName" = resource_names[res_idx_vec],
      "ResCreatedDate" = c_dates[res_idx_vec],
      "ResModifiedDate" = m_dates[res_idx_vec]
    )

    combined <- dplyr::bind_cols(context_info, combined)

    # Remove the temporary index column if it exists (might be missing if combined was empty)
    if ("res_idx" %in% names(combined)) {
      combined[["res_idx"]] <- NULL
    }
  } else {
    # Combine the list of resources into a single tibble
    combined <- purrr::list_rbind(all_data)
  }

  return(combined)
}
