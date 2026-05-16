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
#' @examplesIf isTRUE(length(curl::nslookup("www.opendata.nhs.scot", error =
#' FALSE)) > 0L)
#' \dontrun{
#' get_dataset("gp-practice-populations", max_resources = 2, rows = 10)
#' }
get_dataset <- function(dataset_name,
                        max_resources = NULL,
                        rows = NULL,
                        row_filters = NULL,
                        col_select = NULL,
                        include_context = FALSE) {
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
  res_limit <- if (is.null(max_resources)) n_res else max_resources
  res_index <- seq_len(min(n_res, res_limit))

  selection_ids <- all_ids[res_index]

  # get all resources
  all_data <- purrr::map(
    selection_ids,
    get_resource,
    rows = rows,
    row_filters = row_filters,
    col_select = col_select
  )

  # Identify column type inconsistencies using purrr
  all_col_types <- purrr::map(all_data, \(df) {
    purrr::map_chr(df, \(x) class(x)[1L])
  }) |>
    purrr::list_c()

  if (!is.null(all_col_types)) {
    type_splits <- split(all_col_types, names(all_col_types))
    to_coerce <- names(type_splits)[purrr::map_lgl(type_splits, \(x) {
      length(unique(x)) > 1L
    })]

    if (length(to_coerce) > 0L) {
      cli::cli_warn(c(
        "Due to conflicts between column types across resources, ",
        "the following {cli::qty(to_coerce)} column{?s} ha{?s/ve} been ",
        "coerced to type character:",
        "{.val {to_coerce}}"
      ))

      all_data <- purrr::map(all_data, \(df) {
        for (col in intersect(to_coerce, names(df))) {
          df[[col]] <- as.character(df[[col]])
        }
        df
      })
    }
  }

  # Combine the list of resources into a single tibble
  combined <- purrr::list_rbind(all_data,
    names_to = if (include_context) "res_idx" else NULL
  )

  if (include_context) {
    # Optimized batch context addition
    meta <- content$result$resources[res_index]

    # Extract metadata using purrr
    meta_df <- tibble::tibble(
      res_idx = seq_along(selection_ids),
      ResID = purrr::map_chr(meta, ~ .x$id),
      ResName = purrr::map_chr(meta, ~ .x$name),
      created = purrr::map_chr(meta, ~ .x$created),
      modified = purrr::map_chr(meta, ~ {
        if (is.null(.x$last_modified)) NA_character_ else .x$last_modified
      })
    )

    # Vectorized date parsing and identity correction
    meta_df$ResCreatedDate <- as.POSIXct(meta_df$created,
      format = "%FT%X", tz = "UTC")
    meta_df$ResModifiedDate <- as.POSIXct(meta_df$modified,
      format = "%FT%X", tz = "UTC")

    m_lt_c <- !is.na(meta_df$ResModifiedDate) &
              meta_df$ResModifiedDate < meta_df$ResCreatedDate
    meta_df$ResModifiedDate[m_lt_c] <- meta_df$ResCreatedDate[m_lt_c]

    # Map metadata back to the combined data frame via res_idx
    combined <- combined |>
      dplyr::left_join(
        meta_df |> dplyr::select(res_idx, dplyr::starts_with("Res")),
        by = "res_idx"
      ) |>
      dplyr::select(-res_idx) |>
      dplyr::relocate(dplyr::starts_with("Res"), .before = dplyr::everything())
  }

  combined
}
