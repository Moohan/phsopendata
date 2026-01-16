#' Get Open Data resources from a dataset
#'
#' @param dataset_name name of the dataset as found on
#' \href{https://www.opendata.nhs.scot/}{NHS Open Data platform}
#' @param max_resources (optional) the maximum number of resources
#' to return, use for testing code,
#' it will return the n latest resources
#' @param rows (optional) specify the max number of rows
#' to return for each resource.
#' @inheritParams get_resource
#'
#' @seealso [get_resource()] for downloading a single resource
#' from a dataset.
#'
#' @return a [tibble][tibble::tibble-package] with the data
#' @export
#'
#' @examples get_dataset("gp-practice-populations",
#'   max_resources = 2, rows = 10
#' )
get_dataset <- function(dataset_name,
                        max_resources = NULL,
                        rows = NULL,
                        row_filters = NULL,
                        col_select = NULL,
                        include_context = FALSE) {
  # throw error if name type/format is invalid
  check_dataset_name(dataset_name)

  # define query and try API call
  query <- list("id" = dataset_name)
  content <- try(
    phs_GET("package_show", query),
    silent = TRUE
  )

  # if the API call failed, try to suggest a dataset name
  if (inherits(content, "try-error")) {
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

  # Check for columns that have multiple types across all resources
  # This is much faster than the previous dplyr/purrr implementation as it
  # avoids creating tibbles and uses base R functions that are highly
  # optimized for this kind of operation.
  # 1. `do.call(c, unname(types))` efficiently flattens the list of named
  #    vectors into a single named vector. `unname` is important to avoid
  #    creating compound names like 'list_element.colA'.
  # 2. `split()` groups the types by column name.
  # 3. `vapply()` checks for type inconsistencies in a type-safe manner.
  flattened_types <- do.call(c, unname(types))
  if (length(flattened_types) > 0) {
    grouped_types <- split(unname(flattened_types), names(flattened_types))
    to_coerce <- names(grouped_types)[vapply(grouped_types, function(x) length(unique(x)) > 1, logical(1))]
  } else {
    to_coerce <- character(0)
  }

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
        "created_date" = purrr::map_chr(content$result$resources[res_index], ~ .x$created),
        "modified_date" = purrr::map_chr(content$result$resources[res_index], ~ .x$last_modified)
      ),
      add_context
    )
  }

  # Combine the list of resources into a single tibble
  combined <- purrr::list_rbind(all_data)

  return(combined)
}
