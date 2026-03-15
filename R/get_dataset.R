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

  # Handle connection errors or other phs_GET failures
  if (inherits(content, "try-error")) {
    error_msg <- as.character(content)
    if (grepl("Not Found Error", error_msg, fixed = TRUE)) {
      suggest_dataset_name(dataset_name)
    }
    cli::cli_abort(
      c(
        "Can't connect to the CKAN server.",
        i = "Check your network or proxy settings."
      ),
      call = rlang::caller_env()
    )
  }

  # if content contains a 'Not Found Error'
  # throw error with suggested dataset name
  if (grepl("Not Found Error", content[1L], fixed = TRUE)) {
    suggest_dataset_name(dataset_name)
  }

  n_res <- length(content$result$resources)
  res_index <- seq_len(min(n_res, if (is.null(max_resources)) n_res else max_resources))

  # define list of resource IDs and names to get
  selection_ids <- vapply(content$result$resources[res_index], function(x) x$id, character(1L))

  # get all resources
  all_data <- purrr::map(
    selection_ids,
    get_resource,
    rows = rows,
    row_filters = row_filters,
    col_select = col_select
  )

  # resolve class issues
  # extract the first class of each column for all resources
  all_types <- unlist(
    lapply(all_data, function(df) {
      vapply(df, function(x) class(x)[1L], character(1L))
    }),
    use.names = FALSE
  )

  # extract all column names to group types by column
  all_names <- unlist(lapply(all_data, names), use.names = FALSE)

  # find columns with inconsistent types across resources
  inconsistencies <- split(all_types, all_names)
  to_coerce <- names(
    Filter(function(x) length(unique(x)) > 1L, inconsistencies)
  )

  if (length(to_coerce) > 0L) {
    cli::cli_warn(c(
      "Due to conflicts between column types across resources,
      the following {cli::qty(to_coerce)} column{?s} ha{?s/ve} been coerced to type character:",
      "{.val {to_coerce}}"
    ))

    # Coerce problematic columns to character across all resources
    all_data <- lapply(all_data, function(df) {
      cols_present <- intersect(to_coerce, names(df))
      if (length(cols_present) > 0L) {
        df[cols_present] <- lapply(df[cols_present], as.character)
      }
      return(df)
    })
  }

  # Combine the list of resources into a single tibble
  combined <- purrr::list_rbind(all_data)

  if (include_context) {
    # Add the 'resource context' as columns to the data
    # Create an index to map metadata to each row of the combined data frame
    res_idx <- rep(
      seq_along(all_data),
      vapply(all_data, nrow, integer(1L))
    )

    # Extract metadata once for efficiency
    res_metadata <- content$result$resources[res_index]
    res_names <- vapply(res_metadata, function(x) x$name, character(1L))
    res_created <- vapply(res_metadata, function(x) x$created, character(1L))
    res_modified <- vapply(res_metadata, function(x) {
      if (is.null(x$last_modified)) NA_character_ else x$last_modified
    }, character(1L))

    combined <- add_context(
      data = combined,
      id = selection_ids[res_idx],
      name = res_names[res_idx],
      created_date = res_created[res_idx],
      modified_date = res_modified[res_idx]
    )
  }

  return(combined)
}
