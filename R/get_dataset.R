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

  # if content contains a 'Not Found' error
  # throw error with suggested dataset name
  if (inherits(content, "try-error")) {
    if (grepl("Not Found", content[1])) {
      suggest_dataset_name(dataset_name)
    }

    # If it's another type of error, stop and throw it
    stop(content)
  }

  # define list of resource IDs to get
  all_ids <- vapply(
    content$result$resources,
    function(x) x$id,
    character(1)
  )

  n_res <- length(all_ids)
  res_index <- 1:min(n_res, max_resources)

  selection_ids <- all_ids[res_index]

  # get all resources
  all_data <- purrr::map(
    selection_ids,
    get_resource,
    rows = rows,
    row_filters = row_filters,
    col_select = col_select,
  )

  if (length(all_data) == 0) {
    return(tibble::tibble())
  }

  # resolve class issues
  # Get the first class of each column for all data frames
  all_types_list <- lapply(all_data, function(df) {
    vapply(df, function(col) class(col)[1], character(1))
  })

  # Flatten and group by column name to find inconsistencies
  flat_types <- do.call(c, unname(all_types_list))
  split_types <- split(flat_types, names(flat_types))

  # Identify columns with more than one unique type across resources
  is_inconsistent <- vapply(
    split_types,
    function(x) length(unique(x)) > 1,
    logical(1)
  )

  to_coerce <- names(is_inconsistent)[is_inconsistent]

  if (length(to_coerce) > 0) {
    cli::cli_warn(c(
      "Due to conflicts between column types across resources,
      the following {cli::qty(to_coerce)} column{?s} ha{?s/ve} been coerced to type character:",
      "{.val {to_coerce}}"
    ))

    all_data <- lapply(all_data, function(df) {
      cols <- intersect(to_coerce, names(df))
      if (length(cols) > 0) {
        df[cols] <- lapply(df[cols], as.character)
      }
      df
    })
  }

  # Combine the list of resources into a single tibble
  combined <- purrr::list_rbind(all_data)

  if (include_context) {
    # Add the 'resource context' as columns to the data
    # Pre-calculating metadata in a vectorized way is much faster than
    # calling add_context in a loop.
    n_rows <- vapply(all_data, nrow, integer(1))

    res_ids <- selection_ids
    res_names <- vapply(
      content$result$resources[res_index],
      function(x) x$name,
      character(1)
    )
    res_created <- vapply(
      content$result$resources[res_index],
      function(x) x$created,
      character(1)
    )
    res_modified_list <- lapply(
      content$result$resources[res_index],
      function(x) x$last_modified
    )
    res_modified <- vapply(
      res_modified_list,
      function(x) if (is.null(x)) NA_character_ else x,
      character(1)
    )

    # Parse and handle dates (as in add_context)
    res_created_dates <- as.POSIXct(res_created, format = "%FT%X", tz = "UTC")
    res_modified_dates <- as.POSIXct(res_modified, format = "%FT%X", tz = "UTC")

    # The platform can record the modified date as being before the created date
    # by a few microseconds, this will catch any rounding which ensure
    # created_date is always <= modified_date
    too_early <- !is.na(res_modified_dates) &
      !is.na(res_created_dates) &
      res_modified_dates < res_created_dates
    res_modified_dates[too_early] <- res_created_dates[too_early]

    # Prepend context columns in a vectorized way.
    # We use rep() to expand the metadata for each resource by its number of rows,
    # ensuring the context matches the combined data from list_rbind.
    combined <- dplyr::bind_cols(
      tibble::tibble(
        "ResID" = rep(res_ids, n_rows),
        "ResName" = rep(res_names, n_rows),
        "ResCreatedDate" = rep(res_created_dates, n_rows),
        "ResModifiedDate" = rep(res_modified_dates, n_rows)
      ),
      combined
    )
  }

  return(combined)
}
