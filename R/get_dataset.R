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

  # resolve class issues by finding columns with multiple unique types
  all_types <- unlist(lapply(all_data, function(df) {
    vapply(df, function(x) class(x)[1L], character(1L))
  }), use.names = FALSE)

  all_names <- unlist(lapply(all_data, names), use.names = FALSE)

  inconsistent_cols <- split(all_types, all_names) %>%
    purrr::keep(~ length(unique(.x)) > 1L)

  # define which columns to coerce and warn
  to_coerce <- names(inconsistent_cols)

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
  combined <- purrr::list_rbind(all_data)

  if (include_context) {
    # Extract metadata for selected resources
    metadata <- content$result$resources[res_index]
    ids <- selection_ids
    names <- purrr::map_chr(metadata, ~ .x$name)
    created_dates <- purrr::map_chr(metadata, ~ .x$created)
    modified_dates <- purrr::map_chr(metadata, ~ if (is.null(.x$last_modified)) NA_character_ else .x$last_modified)

    # Pre-parse dates once before expansion
    res_created <- as.POSIXct(created_dates, format = "%FT%X", tz = "UTC")
    res_modified <- as.POSIXct(modified_dates, format = "%FT%X", tz = "UTC")

    # Handle 'modified < created' discrepancy
    fix_idx <- !is.na(res_modified) & res_modified < res_created
    res_modified[fix_idx] <- res_created[fix_idx]

    # Create an index for mapping metadata back to rows
    row_counts <- vapply(all_data, nrow, integer(1L))
    meta_idx <- rep(seq_along(all_data), row_counts)

    # Add context columns vectorized
    context_df <- tibble::tibble(
      ResID = ids[meta_idx],
      ResName = names[meta_idx],
      ResCreatedDate = res_created[meta_idx],
      ResModifiedDate = res_modified[meta_idx]
    )

    combined <- combined[, setdiff(names(combined), names(context_df)), drop = FALSE]
    combined <- dplyr::bind_cols(context_df, combined)
  }

  return(combined)
}
