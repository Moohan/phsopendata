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

  # resolve class issues using vectorized check
  # unlist types without names to avoid name-mangling, then split by col names
  all_cols <- unlist(lapply(all_data, names), use.names = FALSE)
  all_types <- unlist(lapply(all_data, function(df) {
    vapply(df, function(x) class(x)[1L], character(1L))
  }), use.names = FALSE)

  type_splits <- split(all_types, all_cols)
  is_inconsistent <- vapply(type_splits, function(x) {
    length(unique(x)) > 1L
  }, logical(1L))

  # define which columns to coerce and warn
  to_coerce <- names(is_inconsistent)[is_inconsistent]

  if (length(to_coerce) > 0L) {
    cli::cli_warn(c(
      "Due to conflicts between column types across resources,
      the following {cli::qty(to_coerce)} column{?s} ha{?s/ve} been coerced to type character:",
      "{.val {to_coerce}}"
    ))

    # Use lapply for faster coercion across list of data frames
    all_data <- lapply(all_data, function(df) {
      cols_present <- intersect(to_coerce, names(df))
      if (length(cols_present) > 0L) {
        for (col in cols_present) {
          df[[col]] <- as.character(df[[col]])
        }
      }
      return(df)
    })
  }

  # Combine the list of resources into a single tibble
  # use names_to to track resource index for context mapping
  combined <- purrr::list_rbind(
    all_data,
    names_to = if (include_context) "res_idx" else NULL
  )

  if (include_context) {
    # If the combined data is empty, ensure the return is a stable tibble
    if (nrow(combined) == 0L) {
      combined <- combined[, setdiff(names(combined), "res_idx"), drop = FALSE]
      combined <- tibble::add_column(
        combined,
        ResID = character(),
        ResName = character(),
        ResCreatedDate = as.POSIXct(character(), tz = "UTC"),
        ResModifiedDate = as.POSIXct(character(), tz = "UTC"),
        .before = 1L
      )
      return(combined)
    }

    # Extract and pre-parse unique metadata dates to minimize overhead
    res_metadata <- content$result$resources[res_index]
    res_ids <- selection_ids
    res_names <- vapply(res_metadata, function(x) x$name, character(1L))
    res_created_raw <- vapply(res_metadata, function(x) x$created, character(1L))
    res_modified_raw <- vapply(res_metadata, function(x) {
      if (is.null(x$last_modified)) NA_character_ else x$last_modified
    }, character(1L))

    # Pre-parse unique dates
    unique_created_raw <- unique(res_created_raw)
    unique_modified_raw <- unique(res_modified_raw)

    parsed_created <- as.POSIXct(unique_created_raw, format = "%FT%X", tz = "UTC")
    parsed_modified <- as.POSIXct(unique_modified_raw, format = "%FT%X", tz = "UTC")

    # Map back to resource order
    res_created <- parsed_created[match(res_created_raw, unique_created_raw)]
    res_modified <- parsed_modified[match(res_modified_raw, unique_modified_raw)]

    # Vectorized context addition: map metadata to rows via res_idx
    # Use integer indexing into metadata vectors
    res_idx <- as.integer(combined$res_idx)

    combined <- add_context(
      data = combined[, setdiff(names(combined), "res_idx"), drop = FALSE],
      id = res_ids[res_idx],
      name = res_names[res_idx],
      created_date = res_created[res_idx],
      modified_date = res_modified[res_idx]
    )
  }

  # Ensure tibble output even if empty (no resources found)
  if (is.null(combined)) {
    combined <- tibble::tibble()
  }

  return(combined)
}
