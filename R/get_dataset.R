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
  res_index <- seq_len(
    min(n_res, if (is.null(max_resources)) n_res else max_resources)
  )

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
  # Vectorized approach to identify inconsistent column types across resources
  all_names <- unlist(lapply(all_data, names), use.names = FALSE)
  all_types <- unlist(
    lapply(
      all_data,
      function(df) vapply(df, function(x) class(x)[1L], character(1L))
    ),
    use.names = FALSE
  )

  types_by_col <- split(all_types, all_names)
  is_inconsistent <- vapply(
    types_by_col,
    function(x) length(unique(x)) > 1L,
    logical(1L)
  )

  # define which columns to coerce and warn
  to_coerce <- names(is_inconsistent)[is_inconsistent]

  if (length(to_coerce) > 0L) {
    cli::cli_warn(c(
      "Due to conflicts between column types across resources,
      the following {cli::qty(to_coerce)} column{?s} ha{?s/ve} been coerced to type character:",
      "{.val {to_coerce}}"
    ))

    all_data <- lapply(all_data, function(df) {
      cols_to_fix <- intersect(to_coerce, names(df))
      if (length(cols_to_fix) > 0L) {
        df[cols_to_fix] <- lapply(df[cols_to_fix], as.character)
      }
      df
    })
  }

  # Combine the list of resources into a single tibble
  # use names_to to allow vectorized context addition
  combined <- purrr::list_rbind(
    all_data,
    names_to = if (include_context) "res_idx" else NULL
  )

  if (include_context) {
    # Vectorized addition of resource context columns
    res_metadata <- content$result$resources[res_index]

    ids <- selection_ids
    names <- vapply(
      res_metadata,
      function(x) if (is.null(x$name)) NA_character_ else x$name,
      character(1L)
    )
    created_dates <- vapply(
      res_metadata,
      function(x) if (is.null(x$created)) NA_character_ else x$created,
      character(1L)
    )
    modified_dates <- vapply(
      res_metadata,
      function(x) if (is.null(x$last_modified)) NA_character_ else x$last_modified,
      character(1L)
    )

    # Parse dates
    p_created_dates <- as.POSIXct(created_dates, format = "%FT%X", tz = "UTC")
    p_modified_dates <- as.POSIXct(modified_dates, format = "%FT%X", tz = "UTC")

    # Handle cases where modified date is recorded before created date
    mask <- !is.na(p_created_dates) &
      !is.na(p_modified_dates) &
      p_modified_dates < p_created_dates
    p_modified_dates[mask] <- p_created_dates[mask]

    # Map back to rows using integer index
    res_idx <- as.integer(combined$res_idx)

    # Prepend context columns using performant base R assignment
    combined$ResID <- ids[res_idx]
    combined$ResName <- names[res_idx]
    combined$ResCreatedDate <- p_created_dates[res_idx]
    combined$ResModifiedDate <- p_modified_dates[res_idx]

    # Reorder columns to the front and remove temporary index
    context_cols <- c("ResID", "ResName", "ResCreatedDate", "ResModifiedDate")
    combined <- combined[
      ,
      c(context_cols, setdiff(names(combined), c(context_cols, "res_idx")))
    ]
  }

  return(combined)
}
