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
  # Vectorized type consistency check
  all_types <- unlist(
    lapply(all_data, function(df) vapply(df, function(x) class(x)[1L], character(1L))),
    use.names = FALSE
  )
  all_names <- unlist(lapply(all_data, names), use.names = FALSE)

  if (is.null(all_types)) all_types <- character(0L)
  if (is.null(all_names)) all_names <- character(0L)

  types_by_col <- split(all_types, all_names)
  to_coerce <- names(types_by_col)[vapply(types_by_col, function(x) length(unique(x)) > 1L, logical(1L))]

  if (length(to_coerce) > 0L) {
    cli::cli_warn(c(
      "Due to conflicts between column types across resources,
      the following {cli::qty(to_coerce)} column{?s} ha{?s/ve} been coerced to type character:",
      "{.val {to_coerce}}"
    ))

    all_data <- lapply(all_data, function(df) {
      target_cols <- intersect(names(df), to_coerce)
      for (col in target_cols) {
        df[[col]] <- as.character(df[[col]])
      }
      df
    })
  }

  # Combine the list of resources into a single tibble
  combined <- purrr::list_rbind(all_data)

  if (include_context) {
    # Extract and pre-parse metadata
    resources <- content$result$resources[res_index]
    res_names <- purrr::map_chr(resources, ~ .x$name)
    created_dates <- purrr::map_chr(resources, ~ .x$created)
    modified_dates <- purrr::map_chr(
      resources,
      ~ if (is.null(.x$last_modified)) NA_character_ else .x$last_modified
    )

    # Parse dates
    p_created <- as.POSIXct(created_dates, format = "%FT%X", tz = "UTC")
    p_modified <- as.POSIXct(modified_dates, format = "%FT%X", tz = "UTC")

    # Correct modified < created on the metadata vector (highly efficient)
    m_lt_c <- !is.na(p_modified) & !is.na(p_created) & p_modified < p_created
    if (any(m_lt_c)) {
      p_modified[m_lt_c] <- p_created[m_lt_c]
    }

    # Expand metadata to match combined rows
    row_counts <- vapply(all_data, nrow, integer(1L))
    meta_idx <- rep(seq_along(all_data), row_counts)

    combined <- add_context(
      data = combined,
      id = selection_ids[meta_idx],
      name = res_names[meta_idx],
      created_date = p_created[meta_idx],
      modified_date = p_modified[meta_idx]
    )
  }

  # Ensure tibble output
  if (!inherits(combined, "tbl_df")) {
    combined <- tibble::as_tibble(combined)
  }

  return(combined)
}
