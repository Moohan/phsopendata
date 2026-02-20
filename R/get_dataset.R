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
  # Optimization: vapply is faster than purrr::map_chr and handles multi-class
  # objects (like POSIXct) safely by taking the first class name.
  types <- lapply(
    all_data,
    function(df) vapply(df, function(col) class(col)[1], character(1))
  )

  # for each df, check if next df class matches
  inconsistencies <- vector(length = length(types) - 1L, mode = "list")
  for (i in seq_along(types)) {
    if (i == length(types)) break

    this_types <- types[[i]]
    next_types <- types[[i + 1L]]

    # find matching names
    matching_names <- suppressWarnings(
      names(this_types) == names(next_types)
    )

    # of matching name cols, find if types match too
    inconsistent_index <- this_types[matching_names] !=
      next_types[matching_names]
    inconsistencies[[i]] <- this_types[matching_names][inconsistent_index]
  }

  # define which columns to coerce and warn
  to_coerce <- unique(names(unlist(inconsistencies)))

  if (length(to_coerce) > 0L) {
    cli::cli_warn(c(
      "Due to conflicts between column types across resources,
      the following {cli::qty(to_coerce)} column{?s} ha{?s/ve} been coerced to type character:",
      "{.val {to_coerce}}"
    ))

    # Optimization: Base R batch coercion via lapply is significantly faster
    # than dplyr::mutate(across(...)) for large lists of data frames.
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

  if (include_context) {
    # Optimization: Adding context columns in a vectorized way after
    # binding is much faster than adding them to each data frame individually.
    # Add the 'resource context' as columns to the data
    res_metadata <- content$result$resources[res_index]

    res_names <- vapply(
      res_metadata,
      function(x) if (is.null(x$name)) NA_character_ else x$name,
      character(1L)
    )
    res_created <- vapply(
      res_metadata,
      function(x) if (is.null(x$created)) NA_character_ else x$created,
      character(1L)
    )
    res_modified <- vapply(
      res_metadata,
      function(x) if (is.null(x$last_modified)) NA_character_ else x$last_modified,
      character(1L)
    )

    # Parse the date values
    res_created_date <- as.POSIXct(res_created, format = "%FT%X", tz = "UTC")
    res_modified_date <- as.POSIXct(res_modified, format = "%FT%X", tz = "UTC")

    # The platform can record the modified date as being before the created date
    # by a few microseconds, this will catch any rounding which ensure
    # created_date is always <= modified_date
    swap_idx <- !is.na(res_modified_date) & res_modified_date < res_created_date
    res_modified_date[swap_idx] <- res_created_date[swap_idx]

    # Create an index for expansion
    res_counts <- vapply(all_data, nrow, integer(1L))
    idx <- rep(seq_along(all_data), res_counts)

    # Prepend the context columns to the data
    context_data <- tibble::tibble(
      ResID = selection_ids[idx],
      ResName = res_names[idx],
      ResCreatedDate = res_created_date[idx],
      ResModifiedDate = res_modified_date[idx]
    )

    combined <- dplyr::bind_cols(context_data, combined)
  }

  return(combined)
}
