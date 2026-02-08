#' Get Open Data resources from a dataset
#'
#' @description Downloads multiple resources from a dataset on the NHS Open Data platform by dataset name, with optional row limits and context columns.
#'
#' @param dataset_name Name of the dataset as found on \href{https://www.opendata.nhs.scot/}{NHS Open Data platform} (character).
#' @param max_resources (optional) The maximum number of resources to return (integer). If not set, all resources are returned.
#' @inheritParams get_resource
#'
#' @seealso [get_resource()] for downloading a single resource from a dataset.
#'
#' @return A [tibble][tibble::tibble-package] with the data.
#' @export
#'
#' @examples
#' get_dataset("gp-practice-populations", max_resources = 2, rows = 10)
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
  query <- list("id" = dataset_name)
  content <- try(
    phs_GET("package_show", query),
    silent = TRUE
  )

  # if content is a try-error
  if (inherits(content, "try-error")) {
    # if content contains a 'Not Found Error'
    # throw error with suggested dataset name
    if (grepl("Not Found Error", content[1])) {
      suggest_dataset_name(dataset_name)
    }
    # Otherwise, re-throw the original error
    stop(content)
  }

  # define list of resource IDs to get
  resources <- content$result$resources
  all_ids <- vapply(resources, function(x) {
    if (is.null(x$id)) NA_character_ else x$id
  }, character(1))

  n_res <- length(all_ids)
  res_limit <- if (is.null(max_resources)) n_res else max_resources
  res_index <- seq_len(min(n_res, res_limit))

  selection_ids <- all_ids[res_index]

  # get all resources
  all_data <- purrr::map(
    selection_ids,
    get_resource,
    rows = rows,
    row_filters = row_filters,
    col_select = col_select,
  )

  # resolve class issues - use vapply for speed and multi-class robustness
  types <- lapply(all_data, function(df) {
    vapply(df, function(col) class(col)[1], character(1))
  })

  # Check for columns that have multiple types across all resources
  # Flatten and group by column name to identify inconsistencies efficiently
  all_types <- do.call(c, unname(types))

  to_coerce <- character(0)
  if (!is.null(all_types)) {
    split_types <- split(all_types, names(all_types))
    to_coerce <- names(split_types)[vapply(
      split_types,
      function(x) length(unique(x)) > 1,
      logical(1)
    )]
  }

  if (length(to_coerce) > 0) {
    cli::cli_warn(c(
      "Due to conflicts between column types across resources,
      the following {cli::qty(to_coerce)} column{?s} ha{?s/ve} been coerced to type character:",
      "{.val {to_coerce}}"
    ))

    # Fast batch coercion using base R
    all_data <- lapply(all_data, function(df) {
      cols_present <- intersect(to_coerce, names(df))
      if (length(cols_present) > 0) {
        df[cols_present] <- lapply(df[cols_present], as.character)
      }
      df
    })
  }

  if (include_context) {
    # Extract metadata once for context addition
    selection_resources <- resources[res_index]
    selection_names <- vapply(selection_resources, function(x) {
      if (is.null(x$name)) NA_character_ else x$name
    }, character(1))
    selection_created <- vapply(selection_resources, function(x) {
      if (is.null(x$created)) NA_character_ else x$created
    }, character(1))
    selection_modified <- vapply(selection_resources, function(x) {
      if (is.null(x$last_modified)) NA_character_ else x$last_modified
    }, character(1))

    # Add the 'resource context' as columns to the data
    all_data <- purrr::pmap(
      list(
        "data" = all_data,
        "id" = selection_ids,
        "name" = selection_names,
        "created_date" = selection_created,
        "modified_date" = selection_modified
      ),
      add_context
    )
  }

  # Combine the list of resources into a single tibble
  combined <- purrr::list_rbind(all_data)

  return(combined)
}
