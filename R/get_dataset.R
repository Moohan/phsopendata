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
  # Identify type inconsistencies across resources using a vectorized approach.
  # This avoids nested loops and is significantly faster for many resources.
  col_names <- unlist(lapply(all_data, names), use.names = FALSE)
  col_types <- unlist(
    lapply(
      all_data,
      function(df) vapply(df, function(x) class(x)[1L], character(1L))
    ),
    use.names = FALSE
  )

  type_splits <- split(col_types, col_names)
  is_inconsistent <- vapply(
    type_splits,
    function(x) length(unique(x)) > 1L,
    logical(1L)
  )
  to_coerce <- names(is_inconsistent)[is_inconsistent]

  if (length(to_coerce) > 0L) {
    # Coerce inconsistent columns to character across all resources using lapply.
    # Base R batch coercion is faster than dplyr::mutate(across(...)) here.
    cli::cli_warn(c(
      "Due to conflicts between column types across resources,
      the following {cli::qty(to_coerce)} column{?s} ha{?s/ve} been coerced to type character:",
      "{.val {to_coerce}}"
    ))

    all_data <- lapply(all_data, function(df) {
      cols_present <- intersect(to_coerce, names(df))
      if (length(cols_present) > 0L) {
        df[cols_present] <- lapply(df[cols_present], as.character)
      }
      return(df)
    })
  }

  # Combine the list of resources into a single tibble
  combined <- purrr::list_rbind(
    all_data,
    names_to = if (include_context) "res_idx" else NULL
  )

  if (include_context && nrow(combined) > 0L) {
    # Vectorized context addition: apply metadata once to the combined data frame.
    # This is much more efficient than adding context to each resource individually.
    # Extract metadata for all resources
    res_metadata <- content$result$resources[res_index]
    res_idx <- as.integer(combined$res_idx)

    # Pre-parse metadata into vectors to avoid repeated work
    all_ids <- selection_ids[res_idx]
    all_names <- purrr::map_chr(res_metadata, ~ .x$name)[res_idx]
    all_created <- purrr::map_chr(res_metadata, ~ .x$created)[res_idx]
    all_modified <- purrr::map_chr(
      res_metadata,
      ~ if (is.null(.x$last_modified)) NA_character_ else .x$last_modified
    )[res_idx]

    # Add the 'resource context' as columns to the data
    combined <- add_context(
      data = combined,
      id = all_ids,
      name = all_names,
      created_date = all_created,
      modified_date = all_modified
    )

    # Remove the temporary res_idx column
    combined$res_idx <- NULL
  }

  return(combined)
}
