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

  # resolve class issues
  # extract first class only for performance (fixes issues with POSIXct having two classes)
  all_types <- unlist(lapply(all_data, function(df) {
    vapply(df, function(x) class(x)[1], character(1))
  }), use.names = FALSE)

  all_names <- unlist(lapply(all_data, names), use.names = FALSE)

  # Identify columns with more than one unique class
  split_types <- split(all_types, all_names)
  to_coerce <- names(split_types)[vapply(split_types, function(x) length(unique(x)) > 1, logical(1))]

  if (length(to_coerce) > 0L) {
    cli::cli_warn(c(
      "Due to conflicts between column types across resources,
      the following {cli::qty(to_coerce)} column{?s} ha{?s/ve} been coerced to type character:",
      "{.val {to_coerce}}"
    ))

    all_data <- lapply(all_data, function(df) {
      cols_present <- intersect(to_coerce, names(df))
      if (length(cols_present) > 0) {
        for (col in cols_present) {
          df[[col]] <- as.character(df[[col]])
        }
      }
      return(df)
    })
  }

  # Combine the list of resources into a single tibble
  combined <- purrr::list_rbind(all_data, names_to = "res_idx")

  if (include_context) {
    # Add the 'resource context' as columns to the data frame in a vectorized way
    res_meta <- content$result$resources[res_index]
    res_names <- vapply(res_meta, function(x) x$name, character(1))
    res_created <- vapply(res_meta, function(x) x$created, character(1))
    res_modified <- vapply(res_meta, function(x) {
      if (is.null(x$last_modified)) NA_character_ else x$last_modified
    }, character(1))

    # Parse dates only for the unique resources, not every row
    # Use pre-parsed dates to avoid repeated parsing in add_context
    unique_created <- unique(res_created)
    unique_modified <- unique(res_modified)

    created_parsed_map <- stats::setNames(
      as.POSIXct(unique_created, format = "%FT%X", tz = "UTC"),
      unique_created
    )
    modified_parsed_map <- stats::setNames(
      as.POSIXct(unique_modified, format = "%FT%X", tz = "UTC"),
      unique_modified
    )

    created_date_vec <- created_parsed_map[res_created]
    modified_date_vec <- modified_parsed_map[res_modified]
    names(created_date_vec) <- NULL
    names(modified_date_vec) <- NULL

    # Handle 0-row cases where list_rbind doesn't produce res_idx as expected
    if (nrow(combined) > 0) {
      res_idx <- combined$res_idx
      combined <- add_context(
        data = combined,
        id = selection_ids[res_idx],
        name = res_names[res_idx],
        created_date = created_date_vec[res_idx],
        modified_date = modified_date_vec[res_idx]
      )
    } else {
      # Ensure context columns exist even if 0 rows
      combined <- add_context(
        data = combined,
        id = character(),
        name = character(),
        created_date = as.POSIXct(character(), tz = "UTC"),
        modified_date = as.POSIXct(character(), tz = "UTC")
      )
    }
  }

  # Clean up temporary column if it exists
  if ("res_idx" %in% names(combined)) {
    combined$res_idx <- NULL
  }

  return(combined)
}
