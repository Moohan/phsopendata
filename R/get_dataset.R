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
  # Identify type inconsistencies across all resources.
  # Using vapply with class(x)[1L] ensures we only get one class per column (handling POSIXct).
  # Flattening types and names into a single vector and splitting by name is
  # significantly more robust and faster than comparing adjacent resources in a loop.
  all_types <- unlist(lapply(all_data, function(df) {
    vapply(df, function(x) class(x)[1L], character(1L))
  }), use.names = FALSE)

  all_names <- unlist(lapply(all_data, names), use.names = FALSE)

  split_types <- split(all_types, all_names)

  inconsistent <- vapply(
    split_types,
    function(x) length(unique(x)) > 1L,
    logical(1L)
  )

  # define which columns to coerce and warn
  to_coerce <- names(inconsistent)[inconsistent]

  if (length(to_coerce) > 0L) {
    cli::cli_warn(c(
      "Due to conflicts between column types across resources,
      the following {cli::qty(to_coerce)} column{?s} ha{?s/ve} been coerced to type character:",
      "{.val {to_coerce}}"
    ))

    # Batch coercion to character for columns with inconsistent types.
    # Base R lapply is ~25x faster than dplyr::mutate(across(...)) here
    # and significantly reduces memory overhead.
    all_data <- lapply(all_data, function(df) {
      cols_present <- intersect(to_coerce, names(df))
      if (length(cols_present) > 0L) {
        df[cols_present] <- lapply(df[cols_present], as.character)
      }
      df
    })
  }

  if (include_context) {
    # Add the 'resource context' as columns to the data
    all_data <- purrr::pmap(
      list(
        data = all_data,
        id = selection_ids,
        name = purrr::map_chr(content$result$resources[res_index], ~ .x$name),
        created_date = purrr::map_chr(
          content$result$resources[res_index],
          ~ .x$created
        ),
        modified_date = purrr::map_chr(
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
