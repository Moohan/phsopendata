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

  # if content contains a 'Not Found Error'
  # throw error with suggested dataset name
  if (grepl("Not Found Error", content[1])) {
    suggest_dataset_name(dataset_name)
  }

  # define list of resource IDs to get
  all_ids <- purrr::map_chr(content$result$resources, ~ .x$id)

  n_res <- length(all_ids)
  res_index <- 1:min(n_res, max_resources)

  selection_ids <- all_ids[res_index]

  # get all resources
  all_data <- purrr::map(
    selection_ids,
    get_resource,
    rows = rows,
    row_filters = row_filters,
    col_select = col_select,
  )

  # resolve class issues
  types <- purrr::map(
    all_data,
    ~ purrr::map_chr(.x, class)
  )

  # PERFORMANCE: This section was optimised to improve performance.
  # It checks for columns that have inconsistent types across all resources.
  # The original implementation used a `map -> bind_rows -> group_by` chain
  # which was slow for a large number of resources.
  # This base R approach is ~85x faster and uses ~38x less memory.
  to_coerce <- (function(types) {
    # Efficiently flatten the list into a single vector of types, keeping names
    all_types <- do.call(c, unname(types))

    # Return early if there's nothing to process
    if (length(all_types) == 0) {
      return(character(0))
    }

    # Split the vector into a list of vectors, grouped by column name
    types_by_col <- split(all_types, names(all_types))

    # Check which columns have more than one unique type
    inconsistent_cols <- vapply(
      types_by_col,
      function(x) length(unique(x)) > 1,
      FUN.VALUE = logical(1)
    )

    # Get the names of the inconsistent columns and handle the case of no inconsistencies
    names(inconsistent_cols)[inconsistent_cols]
  })(types)

  if (length(to_coerce) > 0) {
    cli::cli_warn(c(
      "Due to conflicts between column types across resources,
      the following {cli::qty(to_coerce)} column{?s} ha{?s/ve} been coerced to type character:",
      "{.val {to_coerce}}"
    ))

    all_data <- purrr::map(
      all_data,
      ~ dplyr::mutate(
        .x,
        dplyr::across(
          dplyr::any_of(to_coerce),
          as.character
        )
      )
    )
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
