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

  # add the id, created date and last_modified to a data.frame
  all_id_data <- tibble::tibble(
    id = vapply(content$result$resources, function(x) x$id, character(1L)),
    created_date = strptime(
      vapply(content$result$resources, function(x) x$created, character(1L)),
      format = "%FT%X",
      tz = "UTC"
    ),
    modified_date = strptime(
      vapply(content$result$resources, function(x) {
        if (is.null(x$last_modified)) NA_character_ else x$last_modified
      }, character(1L)),
      format = "%FT%X",
      tz = "UTC"
    )
  ) %>%
    dplyr::mutate(most_recent_date_created = max(created_date))

  # get the first row of the resources, this will be the same that appears
  # on the top on the open data platform
  all_id_data_first_row <- dplyr::slice_head(all_id_data, n = 1L)

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
