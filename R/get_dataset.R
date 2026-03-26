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
  types <- purrr::map(
    all_data,
    ~ vapply(.x, function(x) class(x)[1], character(1))
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
  combined <- purrr::list_rbind(all_data, names_to = if (include_context) "res_idx" else NULL)

  if (include_context) {
    # Add the 'resource context' as columns to the data
    # We do this after list_rbind for significantly better performance
    res_info <- content$result$resources[res_index]

    # Pre-parse metadata to avoid redundant parsing for every row
    u_id <- selection_ids
    u_name <- purrr::map_chr(res_info, ~ .x$name)
    u_created <- as.POSIXct(
      purrr::map_chr(res_info, ~ .x$created),
      format = "%FT%X", tz = "UTC"
    )
    u_modified <- as.POSIXct(
      purrr::map_chr(res_info, ~ if (is.null(.x$last_modified)) NA_character_ else .x$last_modified),
      format = "%FT%X", tz = "UTC"
    )

    # Fix 'modified < created' discrepancy on the unique metadata
    m_lt_c <- !is.na(u_modified) & u_modified < u_created
    u_modified[m_lt_c] <- u_created[m_lt_c]

    # Map metadata back to all rows using the index from list_rbind
    idx <- as.integer(combined$res_idx)

    combined$ResID <- u_id[idx]
    combined$ResName <- u_name[idx]
    combined$ResCreatedDate <- u_created[idx]
    combined$ResModifiedDate <- u_modified[idx]

    # Reorder columns and remove temporary index
    combined <- combined %>%
      dplyr::select(
        dplyr::all_of(c("ResID", "ResName", "ResCreatedDate", "ResModifiedDate")),
        dplyr::everything(),
        -dplyr::all_of("res_idx")
      )
  }

  return(combined)
}
