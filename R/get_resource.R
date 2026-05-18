#' Get Open Data resource
#'
#' @description Downloads a single resource from the NHS Open Data platform by
#' resource ID, with optional filtering and column selection.
#'
#' @param res_id The resource ID as found on
#' \href{https://www.opendata.nhs.scot/}{NHS Open Data platform} (character).
#' @param rows (optional) Maximum number of rows to return (integer).
#' @param row_filters (optional) A named list or vector specifying values of
#' columns/fields to keep (e.g., list(Date = 20220216, Sex = "Female")).
#' @param col_select (optional) A character vector containing the names of
#' desired columns/fields (e.g., c("Date", "Sex")).
#' @param include_context (optional) If `TRUE`, additional information about the
#' resource will be added as columns to the data, including the resource ID, the
#' resource name, the creation date, and the last modified/updated date.
#'
#' @seealso [get_dataset()] for downloading all resources from a given dataset.
#'
#' @return A [tibble][tibble::tibble-package] with the data.
#' @export
#'
#' @examplesIf isTRUE(length(curl::nslookup("www.opendata.nhs.scot", error =
#' FALSE)) > 0L)
#' res_id <- "ca3f8e44-9a84-43d6-819c-a880b23bd278"
#'
#' data <- get_resource(res_id)
#'
#' filters <- list("HB" = "S08000030", "Month" = "202109")
#' wanted_cols <- c("HB", "Month", "TotalPatientsSeen")
#'
#' filtered_data <- get_resource(
#'   res_id = res_id,
#'   row_filters = filters,
#'   col_select = wanted_cols
#' )
get_resource <- function(res_id,
                         rows = NULL,
                         row_filters = NULL,
                         col_select = NULL,
                         include_context = FALSE) {
  # check res_id
  check_res_id(res_id)

  parsed_col_select <- parse_col_select(col_select)
  parsed_row_filters <- parse_row_filters(row_filters)

  # Branch 1: Use SQL if filters require complex logic
  if (is.logical(parsed_row_filters) &&
        !parsed_row_filters &&
        !is.null(row_filters)) {
    return(get_resource_via_sql(res_id, rows, row_filters, col_select))
  }

  # Branch 2: Standard retrieval modes
  query <- list(
    id = res_id,
    limit = rows,
    q = parsed_row_filters,
    fields = parsed_col_select
  )

  if (use_dump_check(query, rows)) {
    data <- dump_download(res_id)
  } else {
    data <- get_resource_via_datastore(res_id, rows, query)
  }

  if (include_context) {
    data <- add_resource_metadata(data, res_id)
  }

  data
}

# --- Internal Retrieval Strategies ---

get_resource_via_sql <- function(res_id, rows, row_filters, col_select) {
  col_select_sql <- dplyr::if_else(
    is.null(col_select),
    "*",
    paste0("\"", paste(col_select, collapse = "\",\""), "\"")
  )

  row_filters_sql <- paste(
    purrr::imap_chr(
      row_filters,
      function(value, col) {
        paste0("\"", col, "\"=\'", value, "\'", collapse = " OR ")
      }
    ),
    collapse = ") AND ("
  )

  sql <- sprintf(
    "SELECT %s FROM \"%s\" WHERE (%s) %s",
    col_select_sql,
    res_id,
    row_filters_sql,
    dplyr::if_else(is.null(rows), "", paste("LIMIT", rows))
  )

  get_resource_sql(sql)
}

get_resource_via_datastore <- function(res_id, rows, query) {
  if (is.null(query$limit)) query$limit <- 99999L
  query <- purrr::compact(query)

  res_content <- phs_GET("datastore_search", query)
  total_rows <- res_content$result$total

  # Warn if implicit limit hit
  if (is.null(rows) && query$limit < total_rows) {
    cli::cli_warn(c(
      "Returning the first {query$limit} results (rows) of your query. {total_rows} rows match your query in total.",
      i = "To get ALL matching rows you will need to download the whole resource and apply filters/selections locally."
    ))
  }

  # Warn if requested rows not reached
  if (!is.null(rows) && query$limit > total_rows) {
    cli::cli_warn(
      "You set {.var rows} to {.val {rows}} but only {.val {total_rows}} rows matched your query."
    )
  }

  purrr::map(res_content$result$records, ~.x) |>
    dplyr::bind_rows() |>
    dplyr::select(
      -dplyr::starts_with("rank "),
      -dplyr::matches("_id")
    )
}

add_resource_metadata <- function(data, res_id) {
  context_content <- phs_GET(
    action = "resource_show",
    query = paste0("id=", res_id)
  )

  add_context(
    data = data,
    id = context_content$result$id,
    name = context_content$result$name,
    created_date = context_content$result$created,
    modified_date = context_content$result$last_modified
  )
}
