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
  res_index <- seq_len(
    min(n_res, if (is.null(max_resources)) n_res else max_resources)
  )

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
  all_types <- lapply(all_data, function(df) {
    vapply(df, function(col) class(col)[1L], character(1L))
  })

  # Find columns that have different classes across data frames
  first_classes <- list()
  to_coerce <- character()

  for (types in all_types) {
    for (col in names(types)) {
      cls <- types[[col]]
      if (is.null(first_classes[[col]])) {
        first_classes[[col]] <- cls
      } else if (first_classes[[col]] != cls) {
        to_coerce <- c(to_coerce, col)
      }
    }
  }
  to_coerce <- unique(to_coerce)

  if (length(to_coerce) > 0L) {
    cli::cli_warn(c(
      "Due to conflicts between column types across resources,
      the following {cli::qty(to_coerce)} column{?s} ha{?s/ve} been coerced to type character:",
      "{.val {to_coerce}}"
    ))

    all_data <- lapply(all_data, function(df) {
      cols_present <- intersect(to_coerce, names(df))
      if (length(cols_present) > 0L) {
        df[cols_present] <- lapply(df[cols_present], as.character)
      }
      df
    })
  }

  # Combine the list of resources into a single tibble
  combined <- purrr::list_rbind(
    all_data,
    names_to = if (include_context) "res_idx" else NULL
  )

  if (include_context && nrow(combined) > 0L) {
    # Add the 'resource context' as columns to the data
    res_info <- content$result$resources[res_index]

    ids <- selection_ids
    names <- vapply(
      res_info,
      function(x) if (is.null(x$name)) NA_character_ else x$name,
      character(1L)
    )
    created_dates <- vapply(
      res_info,
      function(x) if (is.null(x$created)) NA_character_ else x$created,
      character(1L)
    )
    modified_dates <- vapply(
      res_info,
      function(x) if (is.null(x$last_modified)) NA_character_ else x$last_modified,
      character(1L)
    )

    # Parse the date values
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

    # Handle cases where the modified date is recorded as before the created date
    invalid_modified <- !is.na(modified_dates_posix) &
      !is.na(created_dates_posix) &
      modified_dates_posix < created_dates_posix

    modified_dates_posix[invalid_modified] <- created_dates_posix[invalid_modified]

    # Map to combined data frame
    res_idx <- as.integer(combined$res_idx)

    context_data <- tibble::tibble(
      ResID = ids[res_idx],
      ResName = names[res_idx],
      ResCreatedDate = created_dates_posix[res_idx],
      ResModifiedDate = modified_dates_posix[res_idx]
    )

    # Prepend context columns and remove temporary res_idx
    combined <- dplyr::bind_cols(
      context_data,
      combined[, setdiff(names(combined), "res_idx"), drop = FALSE]
    )
  }

  return(combined)
}
