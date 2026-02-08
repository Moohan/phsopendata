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
#' @examples
#' list_resources("weekly-accident-and-emergency-activity-and-waiting-times")
list_resources <- function(dataset_name) {
  # throw error if name type/format is invalid
  check_dataset_name(dataset_name)

  # define query and try API call
  query <- list("id" = dataset_name)
  content <- try(
    phs_GET("package_show", query),
    silent = TRUE
  )

  # if content is a try-error
  if (inherits(content, "try-error")) {
    # if content contains a 'Not Found Error'
    # throw error with suggested dataset name
    if (grepl("Not Found Error", content[1])) {
      suggest_dataset_name(dataset_name)
    }
    # Otherwise, re-throw the original error
    stop(content)
  }

  # define list of resource IDs names date created and date modified within dataset
  resources <- content$result$resources
  all_ids <- vapply(resources, function(x) {
    if (is.null(x$id)) NA_character_ else x$id
  }, character(1))
  all_names <- vapply(resources, function(x) {
    if (is.null(x$name)) NA_character_ else x$name
  }, character(1))
  all_date_created <- vapply(resources, function(x) {
    if (is.null(x$created)) NA_character_ else x$created
  }, character(1)) %>%
    as.POSIXct(format = "%FT%X", tz = "UTC")
  all_date_modified <- vapply(resources, function(x) {
    if (is.null(x$last_modified)) NA_character_ else x$last_modified
  }, character(1)) %>%
    as.POSIXct(format = "%FT%X", tz = "UTC")
  return_value <- tibble::tibble(
    "res_id" = all_ids,
    "name" = all_names,
    "created" = all_date_created,
    "last_modified" = all_date_modified
  )

  return(return_value)
}
