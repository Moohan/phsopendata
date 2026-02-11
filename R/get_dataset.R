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
  types <- lapply(
    all_data,
    function(df) vapply(df, function(col) class(col)[1L], character(1L))
  )

  # find columns with inconsistent data types across resources
  all_types <- do.call(c, unname(types))

  if (is.null(all_types)) {
    to_coerce <- character(0)
  } else {
    type_list <- split(all_types, names(all_types))
    inconsistent <- vapply(
      type_list,
      function(x) length(unique(x)) > 1L,
      logical(1L)
    )
    to_coerce <- names(inconsistent)[inconsistent]
  }

  if (length(to_coerce) > 0L) {
    cli::cli_warn(c(
      "Due to conflicts between column types across resources,
      the following {cli::qty(to_coerce)} column{?s} ha{?s/ve} been coerced to type character:",
      "{.val {to_coerce}}"
    ))

    all_data <- lapply(all_data, function(df) {
      cols <- intersect(to_coerce, names(df))
      if (length(cols) > 0L) {
        df[cols] <- lapply(df[cols], as.character)
      }
      df
    })
  }

  # Combine the list of resources into a single tibble
  combined <- purrr::list_rbind(all_data)

  if (include_context && nrow(combined) > 0L) {
    # Construct a context tibble by repeating resource metadata
    res_counts <- vapply(all_data, nrow, integer(1L))
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
    res_created <- as.POSIXct(res_created, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
    res_modified <- as.POSIXct(res_modified, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")

    # Ensure created <= modified
    bad_dates <- !is.na(res_modified) & !is.na(res_created) & res_modified < res_created
    res_modified[bad_dates] <- res_created[bad_dates]

    context_data <- tibble::tibble(
      ResID = rep(selection_ids, res_counts),
      ResName = rep(res_names, res_counts),
      ResCreatedDate = rep(res_created, res_counts),
      ResModifiedDate = rep(res_modified, res_counts)
    )

    combined <- dplyr::bind_cols(context_data, combined)
  }

  return(combined)
}
