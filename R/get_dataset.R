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
  content <- tryCatch(
    phs_GET("package_show", query),
    error = function(e) {
      # Handle 'Not Found' error by suggesting a name
      err_msg <- paste(as.character(e), collapse = " ")
      if (grepl("Not Found", err_msg, ignore.case = TRUE)) {
        suggest_dataset_name(dataset_name)
      }
      # Re-throw if it wasn't a Not Found error or if suggestion failed to abort
      stop(e)
    }
  )

  # define list of resource IDs to get
  all_ids <- purrr::map_chr(content$result$resources, ~ .x$id)

  n_res <- length(all_ids)
  res_limit <- if (is.null(max_resources)) n_res else max_resources
  res_index <- seq_len(min(n_res, res_limit))

  selection_ids <- all_ids[res_index]

  # get all resources
  all_data <- purrr::map(
    selection_ids,
    function(id) {
      get_resource(
        res_id = id,
        rows = rows,
        row_filters = row_filters,
        col_select = col_select
      )
    }
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
  # Use names to support names_to in list_rbind
  if (include_context) {
    all_data <- purrr::set_names(all_data, as.character(seq_along(all_data)))
  }

  combined <- purrr::list_rbind(
    all_data,
    names_to = if (include_context) "res_idx" else NULL
  )

  if (include_context) {
    # Optimized batch context addition
    meta <- content$result$resources[res_index]

    # Extract metadata using purrr
    meta_df <- tibble::tibble(
      res_idx = as.character(seq_along(selection_ids)),
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
    meta_cols <- c("res_idx", "ResID", "ResName",
                   "ResCreatedDate", "ResModifiedDate")
    combined <- combined |>
      dplyr::left_join(
        meta_df |> dplyr::select(dplyr::all_of(meta_cols)),
        by = "res_idx"
      ) |>
      dplyr::select(-dplyr::any_of("res_idx")) |>
      dplyr::relocate(dplyr::starts_with("Res"), .before = dplyr::everything())
  }

  combined
}
