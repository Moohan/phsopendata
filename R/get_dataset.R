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

  # If phs_GET failed or returned an unexpected result, handle the error
  if (!is.list(content) || inherits(content, "try-error")) {
    # If it's a character vector (including try-error), check for "Not Found"
    content_str <- if (is.character(content)) content[1] else ""

    # if content contains a 'Not Found' error
    # throw error with suggested dataset name
    if (grepl("Not Found", content_str)) {
      suggest_dataset_name(dataset_name)
    }
    # Otherwise, stop with the original error if it's an error
    if (inherits(content, "try-error")) {
      stop(content)
    } else {
      # Handle cases where it returned something else unexpected
      cli::cli_abort("Unexpected API response structure.", x = content)
    }
  }

  # define list of resource IDs to get
  all_ids <- purrr::map_chr(content$result$resources, ~ .x$id)

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

  # resolve class issues - use a robust and performant base R pattern
  # to find columns with inconsistent types across resources.
  type_list <- lapply(
    all_data,
    function(df) vapply(df, function(col) class(col)[1], character(1))
  )

  # Flatten the list of named character vectors
  all_types <- do.call(c, unname(type_list))

  # Group types by column name and find those with more than one unique type
  types_by_col <- split(all_types, names(all_types))
  to_coerce <- names(types_by_col)[
    vapply(types_by_col, function(x) length(unique(x)) > 1, logical(1))
  ]

  if (length(to_coerce) > 0) {
    cli::cli_warn(c(
      "Due to conflicts between column types across resources,
      the following {cli::qty(to_coerce)} column{?s} ha{?s/ve} been coerced to type character:",
      "{.val {to_coerce}}"
    ))

    # Perform batch coercion using base R for significant performance gains.
    # intersect() ensures we only try to coerce columns that actually exist in the df.
    all_data <- lapply(all_data, function(df) {
      cols_to_fix <- intersect(to_coerce, names(df))
      if (length(cols_to_fix) > 0) {
        df[cols_to_fix] <- lapply(df[cols_to_fix], as.character)
      }
      df
    })
  }

  if (include_context) {
    # Add the 'resource context' as columns to the data
    all_data <- purrr::pmap(
      list(
        "data" = all_data,
        "id" = selection_ids,
        "name" = purrr::map_chr(content$result$resources[res_index], ~ .x$name),
        "created_date" = purrr::map_chr(
          content$result$resources[res_index],
          ~ .x$created
        ),
        "modified_date" = purrr::map_chr(
          content$result$resources[res_index],
          ~ .x$last_modified
        )
      ),
      add_context
    )
  }

  # Combine the list of resources into a single tibble
  combined <- purrr::list_rbind(all_data)

  return(combined)
}
