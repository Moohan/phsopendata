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
  # Use vapply(..., class) to handle multi-class objects like POSIXct robustly
  types_list <- lapply(all_data, function(df) {
    vapply(df, function(col) class(col)[1L], character(1L))
  })

  # Find columns with inconsistent types across resources using a vectorized approach
  all_types <- do.call(c, unname(types_list))
  all_names <- do.call(c, lapply(types_list, names))
  split_types <- split(all_types, all_names)
  to_coerce <- names(split_types)[vapply(split_types, function(x) {
    length(unique(x)) > 1L
  }, logical(1L))]

  if (length(to_coerce) > 0L) {
    cli::cli_warn(c(
      "Due to conflicts between column types across resources,
      the following {cli::qty(to_coerce)} column{?s} ha{?s/ve} been coerced to type character:",
      "{.val {to_coerce}}"
    ))

    # Base R batch coercion is faster than dplyr::mutate(across(...)) in a loop
    all_data <- lapply(all_data, function(df) {
      cols_present <- intersect(to_coerce, names(df))
      if (length(cols_present) > 0L) {
        df[cols_present] <- lapply(df[cols_present], as.character)
      }
      return(df)
    })
  }

  # Combine the list of resources into a single tibble
  # Use names_to to track which row belongs to which resource for context addition
  combined <- purrr::list_rbind(all_data, names_to = "res_idx")

  if (include_context) {
    # Extract metadata using vapply with explicit NULL checks for robustness
    resources <- content$result$resources[res_index]
    res_names <- vapply(resources, function(x) {
      if (is.null(x$name)) NA_character_ else x$name
    }, character(1L))
    res_created <- vapply(resources, function(x) {
      if (is.null(x$created)) NA_character_ else x$created
    }, character(1L))
    res_modified <- vapply(resources, function(x) {
      if (is.null(x$last_modified)) NA_character_ else x$last_modified
    }, character(1L))

    # Vectorized context addition is significantly faster than iterative
    res_idx <- as.integer(combined$res_idx)
    combined <- add_context(
      data = combined[, names(combined) != "res_idx", drop = FALSE],
      id = selection_ids[res_idx],
      name = res_names[res_idx],
      created_date = res_created[res_idx],
      modified_date = res_modified[res_idx]
    )
  } else {
    # Remove the temporary index column if context not requested
    combined$res_idx <- NULL
  }

  return(combined)
}
