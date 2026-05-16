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

  # extract resource metadata using purrr
  resources <- content$result$resources
  all_id_data <- tibble::tibble(
    id = purrr::map_chr(resources, ~ .x$id),
    created_date = as.POSIXct(
      purrr::map_chr(resources, ~ .x$created),
      format = "%FT%X",
      tz = "UTC"
    ),
    modified_date = as.POSIXct(
      purrr::map_chr(resources, ~ .x$last_modified),
      format = "%FT%X",
      tz = "UTC"
    )
  ) |>
    dplyr::mutate(most_recent_date_created = max(created_date))

  # get the first row of the resources
  all_id_data_first_row <- dplyr::slice_head(all_id_data, n = 1L)

  # Check if the top resource has the most recent created date
  if (
    all_id_data_first_row$created_date ==
      all_id_data_first_row$most_recent_date_created
  ) {
    all_id_data_first_row$id
  } else {
    cli::cli_abort("The most recent id could not be identified", call = call)
  }
}
