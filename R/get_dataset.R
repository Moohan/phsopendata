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

  # resolve class issues
  # use vapply for faster and safer class extraction (handles multi-class objects)
  types_list <- lapply(all_data, function(df) {
    vapply(df, function(x) class(x)[1L], character(1L))
  })

  # Flatten and find inconsistencies across ALL data frames
  # split() + vapply(unique) is faster and more robust than pairwise comparison
  all_types <- do.call(c, unname(types_list))
  type_counts <- split(all_types, names(all_types))
  to_coerce <- names(type_counts)[
    vapply(type_counts, function(x) length(unique(x)) > 1L, logical(1L))
  ]

  if (length(to_coerce) > 0L) {
    cli::cli_warn(c(
      "Due to conflicts between column types across resources,
      the following {cli::qty(to_coerce)} column{?s} ha{?s/ve} been coerced to type character:",
      "{.val {to_coerce}}"
    ))

    # Base R batch coercion is significantly faster than dplyr::mutate(across(...))
    all_data <- lapply(all_data, function(df) {
      cols_present <- intersect(to_coerce, names(df))
      if (length(cols_present) > 0L) {
        for (col in cols_present) {
          df[[col]] <- as.character(df[[col]])
        }
      }
      df
    })
  }

  # Combine the list of resources into a single tibble
  # use names_to to facilitate vectorized context addition if requested
  combined <- purrr::list_rbind(
    all_data,
    names_to = if (include_context) "res_idx" else NULL
  )

  if (include_context) {
    # Vectorized addition of resource context is significantly faster
    # than calling add_context() iteratively
    res_idx <- as.integer(combined$res_idx)

    ids <- selection_ids
    names <- vapply(
      content$result$resources[res_index],
      function(x) if (is.null(x$name)) NA_character_ else x$name,
      character(1L)
    )
    created_dates <- vapply(
      content$result$resources[res_index],
      function(x) if (is.null(x$created)) NA_character_ else x$created,
      character(1L)
    )
    modified_dates <- vapply(
      content$result$resources[res_index],
      function(x) if (is.null(x$last_modified)) NA_character_ else x$last_modified,
      character(1L)
    )

    created_dates <- as.POSIXct(created_dates, format = "%FT%X", tz = "UTC")
    modified_dates <- as.POSIXct(modified_dates, format = "%FT%X", tz = "UTC")

    # Handle modified_date < created_date due to microsecond rounding
    m_before_c <- !is.na(modified_dates) & !is.na(created_dates) &
      modified_dates < created_dates
    modified_dates[m_before_c] <- created_dates[m_before_c]

    combined$ResID <- ids[res_idx]
    combined$ResName <- names[res_idx]
    combined$ResCreatedDate <- created_dates[res_idx]
    combined$ResModifiedDate <- modified_dates[res_idx]

    # Reorder columns to place context at the beginning
    context_cols <- c("ResID", "ResName", "ResCreatedDate", "ResModifiedDate")
    original_cols <- setdiff(names(combined), c(context_cols, "res_idx"))
    combined <- combined[, c(context_cols, original_cols)]
  }

  return(combined)
}
