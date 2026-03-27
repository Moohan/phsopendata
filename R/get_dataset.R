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
  res_index <- 1L:min(n_res, if (is.null(max_resources)) n_res else max_resources)

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
  # Vectorized type consistency check
  all_types <- unlist(
    lapply(all_data, function(df) vapply(df, function(x) class(x)[1L], character(1L))),
    use.names = FALSE
  )
  all_names <- unlist(lapply(all_data, names), use.names = FALSE)

  types_by_col <- split(all_types, all_names)
  to_coerce <- names(types_by_col)[vapply(types_by_col, function(x) length(unique(x)) > 1L, logical(1L))]

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
  # Keep track of which rows came from which resource for context addition
  combined <- purrr::list_rbind(all_data, names_to = "res_idx")

  if (include_context) {
    # Extract and pre-parse metadata
    resources <- content$result$resources[res_index]
    res_names <- purrr::map_chr(resources, ~ .x$name)
    created_dates <- purrr::map_chr(resources, ~ .x$created)
    modified_dates <- purrr::map_chr(
      resources,
      ~ if (is.null(.x$last_modified)) NA_character_ else .x$last_modified
    )

    # Date parsing (once)
    p_created <- as.POSIXct(created_dates, format = "%FT%X", tz = "UTC")
    p_modified <- as.POSIXct(modified_dates, format = "%FT%X", tz = "UTC")

    # Expand metadata to match the combined rows using res_idx
    if (nrow(combined) > 0L) {
      res_idx_int <- as.integer(combined$res_idx)
      combined <- add_context(
        data = combined,
        id = selection_ids[res_idx_int],
        name = res_names[res_idx_int],
        created_date = p_created[res_idx_int],
        modified_date = p_modified[res_idx_int]
      )
    } else {
      # Handle 0-row combined data frame
      combined <- add_context(
        data = combined,
        id = character(0),
        name = character(0),
        created_date = p_created[0],
        modified_date = p_modified[0]
      )
    }
  }

  # Clean up temporary res_idx column
  combined$res_idx <- NULL

  return(combined)
}
