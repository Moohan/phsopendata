#' Lists all available resources for a dataset
#'
#' `list_resources()` returns all of the resources associated
#' with a dataset
#'
#' @inheritParams get_dataset
#'
#' @return a [tibble][tibble::tibble-package] with the data
#' @export
#'
#' @examplesIf isTRUE(length(curl::nslookup("www.opendata.nhs.scot", error = FALSE)) > 0L)
#' list_resources("weekly-accident-and-emergency-activity-and-waiting-times")
list_resources <- function(dataset_name) {
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

  # define list of resource IDs names date created and date modified within dataset
  res_metadata <- content$result$resources
  all_ids <- vapply(res_metadata, function(x) x$id, character(1L))
  all_names <- vapply(res_metadata, function(x) x$name, character(1L))
  all_date_created <- as.POSIXct(
    vapply(res_metadata, function(x) x$created, character(1L)),
    format = "%FT%X",
    tz = "UTC"
  )
  all_date_modified <- as.POSIXct(
    vapply(res_metadata, function(x) {
      if (is.null(x$last_modified)) NA_character_ else x$last_modified
    }, character(1L)),
    format = "%FT%X",
    tz = "UTC"
  )

  return_value <- tibble::tibble(
    res_id = all_ids,
    name = all_names,
    created = all_date_created,
    last_modified = all_date_modified
  )

  return(return_value)
}
