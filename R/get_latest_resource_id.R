#' get_latest_resource_id
#'
#' to be confident that the resource returned is the one intended
#' two conditions have to be met. It has to appear at the top of
#' of the resource list as shown on the open data platform.
#' The order they are returned via the api is the same
#' as they appear on the open data platform. It also
#' has to have the most recent date created
#'
#' There are only some datasets that this functionality
#' is relevant to, these are listed within applicable
#' datasets and are the datasets that keep historic
#' resources instead of over writing them.
#'
#' @inheritParams get_dataset
#'
#' @return a string with the resource id
#' @keywords internal
#' @noRd
get_latest_resource_id <- function(dataset_name, call = rlang::caller_env()) {
  # send the api request
  query <- list(id = dataset_name)
  content <- phs_GET("package_show", query)

  resources <- content$result$resources
  if (length(resources) == 0) {
    cli::cli_abort("No resources found for dataset {.val {dataset_name}}", call = call)
  }

  # extract metadata safely
  id <- vapply(resources, function(x) if (is.null(x$id)) "" else x$id, character(1))
  created <- vapply(resources, function(x) if (is.null(x$created)) NA_character_ else x$created, character(1))
  modified <- vapply(resources, function(x) if (is.null(x$last_modified)) NA_character_ else x$last_modified, character(1))

  all_id_data <- tibble::tibble(
    id = id,
    created_date = as.POSIXct(created, format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC"),
    modified_date = as.POSIXct(modified, format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC")
  )

  all_id_data <- dplyr::mutate(
    all_id_data,
    most_recent_date_created = max(created_date, na.rm = TRUE)
  )

  # get the first row of the resources, this will be the same that appears
  # on the top on the open data platform
  all_id_data_first_row <- dplyr::slice_head(all_id_data, n = 1L)

  # If the resource at the top as appearing on the open data platform also has the most
  # recent date created, return it. Otherwise, error
  if (
    !is.na(all_id_data_first_row$created_date) &&
      all_id_data_first_row$created_date ==
        all_id_data_first_row$most_recent_date_created
  ) {
    return(all_id_data_first_row$id)
  }
  cli::cli_abort("The most recent id could not be identified", call = call)
}
