#' Add resource context columns to a data frame
#'
#' @description Adds resource context columns (ID, name, created date, modified date) to a data frame returned from a resource query.
#'
#' @param data A data frame or tibble containing resource data.
#' @param id The resource ID (character).
#' @param name The resource name (character).
#' @param created_date The resource creation date (character or POSIXct).
#' @param modified_date The resource last modified date (character or POSIXct).
#'
#' @return A data frame or tibble with context columns prepended.
#' @noRd
#' @keywords internal
add_context <- function(data, id, name, created_date, modified_date) {
  # Catch if the resource has never been modified
  if (is.null(modified_date)) {
    modified_date <- NA_character_
  }

  # Parse the date values using ISO8601 format to avoid locale dependencies
  created_date <- as.POSIXct(created_date, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
  modified_date <- as.POSIXct(modified_date, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")

  # The platform can record the modified date as being before the created date
  # by a few microseconds, this will catch any rounding which ensure
  # created_date is always <= modified_date
  if (!is.na(modified_date) && !is.na(created_date) && modified_date < created_date) {
    modified_date <- created_date
  }

  # Use bind_cols to prepend columns efficiently
  context_cols <- tibble::tibble(
    "ResID" = id,
    "ResName" = name,
    "ResCreatedDate" = created_date,
    "ResModifiedDate" = modified_date
  )

  return(dplyr::bind_cols(context_cols, data))
}
