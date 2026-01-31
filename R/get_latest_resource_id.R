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
  query <- list("id" = dataset_name)
  content <- phs_GET("package_show", query)

  # Retrieve the resource metadata and add to a dataframe.
  # We use purrr::map_chr for vectorization, which is more efficient
  # than growing vectors in a loop.
  all_id_data <- tibble::tibble(
    id = purrr::map_chr(content$result$resources, ~ .x$id),
    created_date = strptime(
      purrr::map_chr(content$result$resources, ~ .x$created),
      format = "%FT%X",
      tz = "UTC"
    ),
    modified_date = strptime(
      purrr::map_chr(content$result$resources, ~ .x$last_modified),
      format = "%FT%X",
      tz = "UTC"
    )
  ) %>%
    dplyr::mutate(most_recent_date_created = max(created_date))

  # get the first row of the resources, this will be the same that appears on the top
  # on the open data platform
  all_id_data_first_row <- all_id_data %>%
    dplyr::slice(1)

  # If the resource at the top as appearing on the open data platform also has the most
  # recent date created, return it. Otherwise, error
  if (
    all_id_data_first_row$created_date ==
      all_id_data_first_row$most_recent_date_created
  ) {
    return(all_id_data_first_row$id)
  }
  cli::cli_abort("The most recent id could not be identified", call = call)
}
