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

  if (length(all_data) == 0L) {
    return(tibble::tibble())
  }

  # identify type inconsistencies across resources
  all_cols <- unlist(lapply(all_data, names), use.names = FALSE)
  all_types <- unlist(lapply(all_data, function(df) {
    vapply(df, function(x) class(x)[1L], character(1L))
  }), use.names = FALSE)

  # split types by column names and find columns with more than one unique type
  types_by_col <- split(all_types, all_cols)
  to_coerce <- names(types_by_col)[vapply(
    types_by_col,
    function(x) length(unique(x)) > 1L,
    logical(1L)
  )]

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
  # Use res_idx to map context if required
  combined <- purrr::list_rbind(
    all_data,
    names_to = if (include_context) "res_idx" else NULL
  )

  if (include_context) {
    # Add the 'resource context' as columns to the data in a single vectorized step
    res_metadata <- content$result$resources[res_index]

    # Pre-parse dates to avoid redundant parsing for every row
    # last_modified can be NULL in CKAN API
    created_dates <- vapply(
      res_metadata,
      function(x) x$created,
      character(1L)
    )
    modified_dates <- vapply(
      res_metadata,
      function(x) if (is.null(x$last_modified)) NA_character_ else x$last_modified,
      character(1L)
    )

    created_dates_ct <- as.POSIXct(created_dates, format = "%FT%X", tz = "UTC")
    modified_dates_ct <- as.POSIXct(modified_dates, format = "%FT%X", tz = "UTC")

    # Fix date discrepancies before expansion
    m_lt_c <- !is.na(modified_dates_ct) &
      !is.na(created_dates_ct) &
      modified_dates_ct < created_dates_ct
    modified_dates_ct[m_lt_c] <- created_dates_ct[m_lt_c]

    res_names <- vapply(res_metadata, function(x) x$name, character(1L))
    res_ids <- selection_ids

    if (nrow(combined) > 0L) {
      # Map metadata back to rows using the resource index
      # names_to in list_rbind returns names if they exist, or 1, 2, 3 as characters
      # since selection_ids is unnamed, it will be characters "1", "2", ...
      idx <- as.integer(combined$res_idx)

      combined <- add_context(
        data = combined,
        id = res_ids[idx],
        name = res_names[idx],
        created_date = created_dates_ct[idx],
        modified_date = modified_dates_ct[idx]
      )
    } else {
      # Handle 0-row case to ensure consistent output schema
      combined <- add_context(
        data = combined,
        id = character(0L),
        name = character(0L),
        created_date = created_dates_ct[0L],
        modified_date = modified_dates_ct[0L]
      )
    }

    combined$res_idx <- NULL
  }

  return(combined)
}
