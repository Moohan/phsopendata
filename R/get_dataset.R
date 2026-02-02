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
  # Get first class of each column for all data frames
  types <- lapply(all_data, function(df) vapply(df, function(col) class(col)[1], character(1)))

  # Check for columns that have multiple types across all resources
  # Flatten the list of named vectors efficiently
  all_types <- do.call(c, unname(types))

  # Group by name and check for unique types
  type_groups <- split(all_types, names(all_types))

  # Find columns with more than one unique type
  is_inconsistent <- vapply(type_groups, function(x) length(unique(x)) > 1, logical(1))

  to_coerce <- names(is_inconsistent)[is_inconsistent]

  if (length(to_coerce) > 0) {
    cli::cli_warn(c(
      "Due to conflicts between column types across resources,
      the following {cli::qty(to_coerce)} column{?s} ha{?s/ve} been coerced to type character:",
      "{.val {to_coerce}}"
    ))

    all_data <- lapply(all_data, function(df) {
      cols_to_coerce <- intersect(names(df), to_coerce)
      if (length(cols_to_coerce) > 0) {
        df[cols_to_coerce] <- lapply(df[cols_to_coerce], as.character)
      }
      df
    })
  }

  # Combine the list of resources into a single tibble
  if (include_context) names(all_data) <- selection_ids
  combined <- purrr::list_rbind(all_data, names_to = if (include_context) "ResID")

  if (include_context) {
    # Add the 'resource context' as columns to the data
    context_table <- tibble::tibble(
      "ResID" = selection_ids,
      "ResName" = purrr::map_chr(content$result$resources[res_index], ~ .x$name),
      "ResCreatedDate" = purrr::map_chr(
        content$result$resources[res_index],
        ~ .x$created
      ),
      "ResModifiedDate" = purrr::map_chr(
        content$result$resources[res_index],
        ~ if (is.null(.x$last_modified)) NA_character_ else .x$last_modified
      )
    )

    # Parse dates and handle modified < created logic (same as in add_context)
    context_table <- context_table %>%
      dplyr::mutate(
        ResCreatedDate = as.POSIXct(ResCreatedDate, format = "%FT%X", tz = "UTC"),
        ResModifiedDate = as.POSIXct(ResModifiedDate, format = "%FT%X", tz = "UTC"),
        ResModifiedDate = dplyr::if_else(
          !is.na(ResModifiedDate) & ResModifiedDate < ResCreatedDate,
          ResCreatedDate,
          ResModifiedDate
        )
      )

    # Join context to combined data
    combined <- combined %>%
      dplyr::left_join(context_table, by = "ResID") %>%
      dplyr::select(
        dplyr::any_of(c("ResID", "ResName", "ResCreatedDate", "ResModifiedDate")),
        dplyr::everything()
      )
  }

  return(combined)
}
