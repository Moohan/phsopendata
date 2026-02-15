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
  # Safe extraction using vapply to handle potential NULL values
  all_ids <- vapply(
    content$result$resources,
    function(x) if (is.null(x$id)) NA_character_ else x$id,
    character(1)
  )

  n_res <- length(all_ids)
  # Handle NULL/zero cases robustly for resource indexing
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

  # Resolve class issues across resources
  # Optimized vectorized approach: flatten and split type vectors
  all_types <- lapply(all_data, function(df) {
    vapply(df, function(col) class(col)[1], character(1))
  })
  flat_types <- do.call(c, unname(all_types))
  split_types <- split(flat_types, names(flat_types))

  # Identify columns with more than one unique class
  to_coerce <- names(split_types)[vapply(
    split_types,
    function(x) length(unique(x)) > 1,
    logical(1)
  )]

  if (length(to_coerce) > 0L) {
    cli::cli_warn(c(
      "Due to conflicts between column types across resources,
      the following {cli::qty(to_coerce)} column{?s} ha{?s/ve} been coerced to type character:",
      "{.val {to_coerce}}"
    ))

    # Base R batch coercion is significantly faster than dplyr::mutate(across())
    all_data <- lapply(all_data, function(df) {
      cols <- intersect(to_coerce, names(df))
      if (length(cols) > 0) {
        df[cols] <- lapply(df[cols], as.character)
      }
      df
    })
  }

  # Combine the list of resources into a single tibble
  # Vectorized context addition after combining is significantly faster
  combined <- purrr::list_rbind(
    all_data,
    names_to = if (include_context) "res_idx" else NULL
  )

  if (include_context) {
    # Extract metadata safely from API response
    res_names <- vapply(
      content$result$resources[res_index],
      function(x) if (is.null(x$name)) NA_character_ else x$name,
      character(1)
    )
    res_created <- vapply(
      content$result$resources[res_index],
      function(x) if (is.null(x$created)) NA_character_ else x$created,
      character(1)
    )
    res_modified <- vapply(
      content$result$resources[res_index],
      function(x) if (is.null(x$last_modified)) NA_character_ else x$last_modified,
      character(1)
    )

    # Parse and fix dates in a vectorized way
    # Use robust ISO8601 format that handles sub-seconds
    created_date <- as.POSIXct(res_created, format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC")
    modified_date <- as.POSIXct(res_modified, format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC")

    # Fix modified dates that appear earlier than created dates due to rounding
    too_early <- !is.na(modified_date) & !is.na(created_date) &
      modified_date < created_date
    modified_date[too_early] <- created_date[too_early]

    # Map res_idx back to indices and extract metadata
    # list_rbind uses indices (as character) for names_to if the list is unnamed
    res_indices <- as.integer(combined$res_idx)

    # Prepend context columns
    combined <- dplyr::bind_cols(
      ResID = selection_ids[res_indices],
      ResName = res_names[res_indices],
      ResCreatedDate = created_date[res_indices],
      ResModifiedDate = modified_date[res_indices],
      combined
    )

    # Remove the index column
    combined$res_idx <- NULL
  }

  return(combined)
}
