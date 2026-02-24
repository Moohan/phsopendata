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
  res_index <- seq_len(min(
    n_res,
    if (is.null(max_resources)) n_res else max_resources
  ))

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
  all_types <- unlist(lapply(all_data, function(df) {
    vapply(df, function(col) class(col)[1L], character(1L))
  }), use.names = FALSE)

  all_names <- unlist(lapply(all_data, names), use.names = FALSE)

  # Find columns with more than one unique type
  type_splits <- split(all_types, all_names)

  # define which columns to coerce and warn
  to_coerce <- names(type_splits)[vapply(
    type_splits,
    function(x) length(unique(x)) > 1L,
    logical(1L)
  )]

  if (length(to_coerce) > 0L) {
    cli::cli_warn(c(
      "Due to conflicts between column types across resources,
      the following {cli::qty(to_coerce)} column{?s} ha{?s/ve} been coerced to type character:",
      "{.val {to_coerce}}"
    ))

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

  # Ensure a tibble is returned even if no resources were found/requested
  if (is.null(combined)) {
    combined <- tibble::tibble()
  }

  if (include_context) {
    # Add the 'resource context' as columns to the data
    res_rows <- vapply(all_data, nrow, integer(1L))
    res_idx <- rep(seq_along(all_data), res_rows)

    res_resources <- content$result$resources[res_index]

    res_names <- vapply(
      res_resources,
      function(x) if (is.null(x$name)) NA_character_ else x$name,
      character(1L)
    )
    res_created <- as.POSIXct(
      vapply(
        res_resources,
        function(x) if (is.null(x$created)) NA_character_ else x$created,
        character(1L)
      ),
      format = "%FT%X",
      tz = "UTC"
    )
    res_modified <- as.POSIXct(
      vapply(
        res_resources,
        function(x) if (is.null(x$last_modified)) NA_character_ else x$last_modified,
        character(1L)
      ),
      format = "%FT%X",
      tz = "UTC"
    )

    combined <- add_context(
      data = combined,
      id = selection_ids[res_idx],
      name = res_names[res_idx],
      created_date = res_created[res_idx],
      modified_date = res_modified[res_idx]
    )
  }

  return(combined)
}
