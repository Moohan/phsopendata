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
    if (grepl("Not Found Error", as.character(content), fixed = TRUE)) {
      suggest_dataset_name(dataset_name)
    } else {
      cli::cli_abort("Failed to fetch dataset metadata for {.val {dataset_name}}.",
                     parent = attr(content, "condition"))
    }
  }

  if (is.null(content$result)) {
     cli::cli_abort("API returned an unexpected response for {.val {dataset_name}}.")
  }

  # define list of resource IDs to get
  all_ids <- vapply(content$result$resources, function(x) x$id, character(1))

  n_res <- length(all_ids)
  res_limit <- if (is.null(max_resources)) n_res else max_resources
  res_index <- seq_len(min(n_res, res_limit))

  if (length(res_index) == 0) {
    return(tibble::tibble())
  }

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
  # Use first class for comparison (handles POSIXct which returns multiple)
  types <- purrr::map(
    all_data,
    function(df) vapply(df, function(x) class(x)[1L], character(1L))
  )

  # for each df, check if next df class matches
  inconsistencies <- list()
  for (i in seq_along(types)) {
    if (i == length(types)) break

    this_types <- types[[i]]
    next_types <- types[[i + 1L]]

    # find matching names
    common_names <- intersect(names(this_types), names(next_types))

    # of matching name cols, find if types match too
    if (length(common_names) > 0) {
      inconsistent_cols <- common_names[this_types[common_names] != next_types[common_names]]
      if (length(inconsistent_cols) > 0) {
        inconsistencies <- c(inconsistencies, inconsistent_cols)
      }
    }
  }

  # define which columns to coerce and warn
  to_coerce <- unique(unlist(inconsistencies))

  if (length(to_coerce) > 0L) {
    cli::cli_warn(c(
      "Due to conflicts between column types across resources,
      the following {cli::qty(to_coerce)} column{?s} ha{?s/ve} been coerced to type character:",
      "{.val {to_coerce}}"
    ))

    all_data <- purrr::map(
      all_data,
      function(df) {
        # Using loop for robust coercion on tibbles
        for (col in intersect(names(df), to_coerce)) {
          df[[col]] <- as.character(df[[col]])
        }
        df
      }
    )
  }

  if (include_context) {
    # Extract metadata safely
    res_list <- content$result$resources[res_index]
    res_names <- vapply(res_list, function(x) if(is.null(x$name)) "" else x$name, character(1))
    res_created <- vapply(res_list, function(x) if(is.null(x$created)) NA_character_ else x$created, character(1))
    res_modified <- vapply(res_list, function(x) if(is.null(x$last_modified)) NA_character_ else x$last_modified, character(1))

    # Add the 'resource context' as columns to the data
    all_data <- purrr::pmap(
      list(
        data = all_data,
        id = selection_ids,
        name = res_names,
        created_date = res_created,
        modified_date = res_modified
      ),
      add_context
    )
  }

  # Combine the list of resources into a single tibble
  combined <- purrr::list_rbind(all_data)

  return(combined)
}
