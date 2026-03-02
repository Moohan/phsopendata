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
  if (inherits(content, "try-error")) {
    if (grepl("Not Found Error", content[1L], fixed = TRUE)) {
      suggest_dataset_name(dataset_name)
    } else {
      # Re-throw other errors
      stop(content)
    }
  }

  # define list of resource IDs to get
  all_ids <- vapply(
    content$result$resources,
    function(x) if (is.null(x$id)) NA_character_ else x$id,
    character(1L)
  )

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
  # Extract first class of each column for each data frame
  # Using vapply here avoids errors when a column has multiple classes (e.g., POSIXct)
  all_types <- lapply(all_data, function(df) {
    vapply(df, function(x) class(x)[1L], character(1L))
  })

  # Flatten all types and names to identify inconsistencies across all resources
  # This approach is significantly faster and more robust than nested loops
  flat_types <- unlist(all_types, use.names = FALSE)
  flat_names <- unlist(lapply(all_types, names), use.names = FALSE)

  # Group types by column name and find those with more than one unique type
  type_splits <- split(flat_types, flat_names)
  to_coerce <- names(type_splits)[
    vapply(type_splits, function(x) length(unique(x)) > 1L, logical(1L))
  ]

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
  # If include_context is TRUE, we use names_to to track resource origin for vectorization
  combined <- purrr::list_rbind(
    all_data,
    names_to = if (include_context) "res_idx" else NULL
  )

  if (include_context) {
    # Pre-parse metadata for all resources
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

    # Pre-parse dates once before recycling to avoid repeated parsing overhead
    res_created_parsed <- as.POSIXct(res_created, format = "%FT%X", tz = "UTC")
    res_modified_parsed <- as.POSIXct(res_modified, format = "%FT%X", tz = "UTC")

    # Map metadata to rows in the combined data frame using the resource index
    res_idx_vector <- combined$res_idx
    combined$res_idx <- NULL # remove temporary index column

    combined <- add_context(
      data = combined,
      id = selection_ids[res_idx_vector],
      name = res_names[res_idx_vector],
      created_date = res_created_parsed[res_idx_vector],
      modified_date = res_modified_parsed[res_idx_vector]
    )
  }

  return(combined)
}
