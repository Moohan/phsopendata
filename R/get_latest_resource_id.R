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
  id <- vector("character")
  created_date <- vector("character")
  modified_date <- vector("character")

  for (res in content$result$resources) {
    id <- append(id, res$id)
    # Ensure created date is not NULL
    res_created <- if (is.null(res$created)) NA_character_ else res$created
    created_date <- append(created_date, res_created)
    # Ensure modified date is not NULL
    res_modified <- if (is.null(res$last_modified)) NA_character_ else res$last_modified
    modified_date <- append(modified_date, res_modified)
  }
  all_id_data <- tibble::tibble(
    id = id,
    created_date = strptime(created_date, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC"),
    modified_date = strptime(modified_date, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
  )

  # Explicitly convert to POSIXct to avoid strptime issues in tibble
  all_id_data$created_date <- as.POSIXct(all_id_data$created_date)
  all_id_data$modified_date <- as.POSIXct(all_id_data$modified_date)

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
