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
  # extract class of each column for each data frame
  types <- lapply(all_data, function(df) {
    vapply(df, function(x) class(x)[1L], character(1L))
  })

  # Find columns that have inconsistent types across resources
  all_names <- unlist(lapply(types, names), use.names = FALSE)
  all_types <- unlist(types, use.names = FALSE)

  split_types <- split(all_types, all_names)

  has_multiple <- vapply(
    split_types,
    function(x) length(unique(x)) > 1L,
    logical(1L)
  )

  # define which columns to coerce and warn
  to_coerce <- names(has_multiple)[has_multiple]

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
  combined <- purrr::list_rbind(all_data, names_to = if (include_context) "res_idx")

  if (include_context) {
    # Add the 'resource context' as columns to the data
    selection_names <- purrr::map_chr(
      content$result$resources[res_index],
      ~ .x$name
    )
    selection_created <- purrr::map_chr(
      content$result$resources[res_index],
      ~ .x$created
    )
    selection_modified <- purrr::map_chr(
      content$result$resources[res_index],
      ~ if (is.null(.x$last_modified)) NA_character_ else .x$last_modified
    )

    # Pre-parse dates once to avoid repeated parsing for every row
    p_created <- as.POSIXct(selection_created, format = "%FT%X", tz = "UTC")
    p_modified <- as.POSIXct(selection_modified, format = "%FT%X", tz = "UTC")
    # Correct modified dates that appear before created dates
    p_modified <- dplyr::if_else(
      !is.na(p_modified) & p_modified < p_created,
      p_created,
      p_modified
    )

    # Map the context to the combined data frame using the resource index
    res_idx <- as.integer(combined$res_idx)
    combined <- combined %>%
      dplyr::mutate(
        ResID = selection_ids[res_idx],
        ResName = selection_names[res_idx],
        ResCreatedDate = p_created[res_idx],
        ResModifiedDate = p_modified[res_idx],
        res_idx = NULL,
        .before = dplyr::everything()
      )
  }

  return(combined)
}
