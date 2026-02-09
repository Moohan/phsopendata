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
  res_limit <- if (is.null(max_resources)) n_res else max_resources
  res_index <- seq_len(min(n_res, res_limit))

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
  # Get classes for all columns in all data frames
  all_types_list <- lapply(all_data, function(df) {
    vapply(df, function(col) class(col)[1L], character(1L))
  })

  # Flatten and group by column name
  all_types_vec <- do.call(c, unname(all_types_list))
  all_types_split <- split(all_types_vec, names(all_types_vec))

  # Find columns with more than one unique type
  is_inconsistent <- vapply(
    all_types_split,
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

    all_data <- lapply(all_data, function(df) {
      cols_present <- intersect(to_coerce, names(df))
      if (length(cols_present) > 0L) {
        df[cols_present] <- lapply(df[cols_present], as.character)
      }
      df
    })
  }

  # Combine the list of resources into a single tibble
  combined <- purrr::list_rbind(all_data)

  if (include_context) {
    # Add the 'resource context' as columns to the data
    n_rows <- vapply(all_data, nrow, integer(1L))

    res_names <- vapply(
      content$result$resources[res_index],
      function(x) if (is.null(x$name)) NA_character_ else x$name,
      character(1L)
    )
    res_created <- vapply(
      content$result$resources[res_index],
      function(x) if (is.null(x$created)) NA_character_ else x$created,
      character(1L)
    )
    res_modified <- vapply(
      content$result$resources[res_index],
      function(x) if (is.null(x$last_modified)) NA_character_ else x$last_modified,
      character(1L)
    )

    # Parse dates
    res_created_date <- as.POSIXct(res_created, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
    res_modified_date <- as.POSIXct(res_modified, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")

    # The platform can record the modified date as being before the created date
    # by a few microseconds, this will catch any rounding which ensure
    # created_date is always <= modified_date
    swap <- !is.na(res_modified_date) & res_modified_date < res_created_date
    res_modified_date[swap] <- res_created_date[swap]

    context_data <- tibble::tibble(
      ResID = rep(selection_ids, n_rows),
      ResName = rep(res_names, n_rows),
      ResCreatedDate = rep(res_created_date, n_rows),
      ResModifiedDate = rep(res_modified_date, n_rows)
    )

    combined <- dplyr::bind_cols(context_data, combined)
  }

  return(combined)
}
