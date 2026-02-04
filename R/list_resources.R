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

  # if content contains a 'Not Found' error
  # throw error with suggested dataset name
  if (inherits(content, "try-error")) {
    if (grepl("Not Found", content[1])) {
      suggest_dataset_name(dataset_name)
    }

    # If it's another type of error, stop and throw it
    stop(content)
  }

  # define list of resource IDs names date created and date modified within dataset
  all_ids <- vapply(
    content$result$resources,
    function(x) x$id,
    character(1)
  )
  all_names <- vapply(
    content$result$resources,
    function(x) x$name,
    character(1)
  )
  all_date_created <- vapply(
    content$result$resources,
    function(x) x$created,
    character(1)
  ) %>%
    as.POSIXct(format = "%FT%X", tz = "UTC")

  all_date_modified_list <- lapply(
    content$result$resources,
    function(x) x$last_modified
  )
  all_date_modified <- vapply(
    all_date_modified_list,
    function(x) if (is.null(x)) NA_character_ else x,
    character(1)
  ) %>%
    as.POSIXct(format = "%FT%X", tz = "UTC")
  return_value <- tibble::tibble(
    "res_id" = all_ids,
    "name" = all_names,
    "created" = all_date_created,
    "last_modified" = all_date_modified
  )

  return(return_value)
}
