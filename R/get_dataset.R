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

  # identify column types to check for inconsistencies
  # extract first class of each column for all dataframes
  all_types <- unlist(
    lapply(
      all_data,
      function(df) vapply(df, function(x) class(x)[1L], character(1L))
    ),
    use.names = FALSE
  )
  all_names <- unlist(lapply(all_data, names), use.names = FALSE)

  # group types by column name
  type_groups <- split(all_types, all_names)

  # find columns with more than one unique type
  to_coerce <- names(type_groups)[
    vapply(type_groups, function(x) length(unique(x)) > 1L, logical(1L))
  ]

  if (length(to_coerce) > 0L) {
    cli::cli_warn(c(
      "Due to conflicts between column types across resources,
      the following {cli::qty(to_coerce)} column{?s} ha{?s/ve} been coerced to type character:",
      "{.val {to_coerce}}"
    ))

    all_data <- purrr::map(
      all_data,
      dplyr::mutate,
      dplyr::across(
        dplyr::any_of(to_coerce),
        as.character
      )
    )
  }

  # Combine the list of resources into a single tibble
  combined <- purrr::list_rbind(
    all_data,
    names_to = if (include_context) "res_idx" else NULL
  )

  if (include_context) {
    # add the 'resource context' as columns to the data
    # pre-parsing dates to avoid redundant parsing for every row
    # of the combined data frame
    res_names <- vapply(
      content$result$resources[res_index],
      function(x) x$name,
      character(1L)
    )
    res_created <- as.POSIXct(
      vapply(
        content$result$resources[res_index],
        function(x) x$created,
        character(1L)
      ),
      format = "%FT%X",
      tz = "UTC"
    )
    res_modified <- as.POSIXct(
      vapply(
        content$result$resources[res_index],
        function(x) if (is.null(x$last_modified)) NA_character_ else x$last_modified,
        character(1L)
      ),
      format = "%FT%X",
      tz = "UTC"
    )

    # ensure created_date is always <= modified_date
    fix_idx <- !is.na(res_modified) & res_modified < res_created
    res_modified[fix_idx] <- res_created[fix_idx]

    # map resource index back to metadata
    idx <- as.integer(combined$res_idx)

    combined$ResID <- selection_ids[idx]
    combined$ResName <- res_names[idx]
    combined$ResCreatedDate <- res_created[idx]
    combined$ResModifiedDate <- res_modified[idx]

    # reorder columns to put context first and remove res_idx
    nc <- ncol(combined)
    combined <- combined[, c((nc - 3L):nc, 2:(nc - 4L)), drop = FALSE]
  }

  return(combined)
}
