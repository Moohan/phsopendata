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
  # unlist all column types across all data frames
  # using use.names = FALSE for speed
  all_types <- unlist(lapply(all_data, function(df) {
    vapply(df, function(x) class(x)[1], character(1))
  }), use.names = FALSE)

  # also extract all column names across all data frames
  all_names <- unlist(lapply(all_data, names), use.names = FALSE)

  # group types by column name
  type_groups <- split(all_types, all_names)

  # find columns with more than one unique type
  is_inconsistent <- vapply(type_groups, function(x) length(unique(x)) > 1, logical(1))
  to_coerce <- names(is_inconsistent)[is_inconsistent]

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
  # include resource index if context is required
  combined <- purrr::list_rbind(
    all_data,
    names_to = if (include_context) "res_idx" else NULL
  )

  if (include_context) {
    # Add the 'resource context' as columns to the data
    # Extract and pre-parse unique dates to avoid redundant parsing
    res_names <- purrr::map_chr(content$result$resources[res_index], ~ .x$name)
    res_createds_raw <- purrr::map_chr(
      content$result$resources[res_index],
      ~ .x$created
    )
    res_modifieds_raw <- purrr::map_chr(
      content$result$resources[res_index],
      ~ if (is.null(.x$last_modified)) NA_character_ else .x$last_modified
    )

    # Pre-parse dates once before expansion
    res_createds <- as.POSIXct(res_createds_raw, format = "%FT%X", tz = "UTC")
    res_modifieds <- as.POSIXct(res_modifieds_raw, format = "%FT%X", tz = "UTC")

    # Map context to the combined data frame using the resource index
    res_idx <- as.integer(combined$res_idx)
    combined$res_idx <- NULL

    combined <- add_context(
      data = combined,
      id = selection_ids[res_idx],
      name = res_names[res_idx],
      created_date = res_createds[res_idx],
      modified_date = res_modifieds[res_idx]
    )
  }

  return(combined)
}
