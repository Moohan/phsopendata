#' Add resource context columns to a data frame
#'
#' @description Adds resource context columns (ID, name, created date, modified
#' date) to a data frame returned from a resource query.
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
  # Catch if the data is NULL (can happen if list_rbind() received empty list)
  if (is.null(data)) {
    return(NULL)
  }

  # Catch if dates are NULL (can happen if passed from get_resource)
  if (is.null(created_date)) created_date <- NA_character_
  if (is.null(modified_date)) modified_date <- NA_character_

  # Parse the date values if they are not already POSIXct
  if (!inherits(created_date, "POSIXct")) {
    created_date <- as.POSIXct(created_date, format = "%FT%X", tz = "UTC")
  }
  if (!inherits(modified_date, "POSIXct")) {
    modified_date <- as.POSIXct(modified_date, format = "%FT%X", tz = "UTC")
  }

  # The platform can record the modified date as being before the created date
  # by a few microseconds, this will catch any rounding which ensure
  # created_date is always <= modified_date
  too_early <- !is.na(modified_date) & !is.na(created_date) &
    modified_date < created_date
  if (any(too_early, na.rm = TRUE)) {
    modified_date[too_early] <- created_date[too_early]
  }

  # Use base R for performance - adds at the end
  data$ResID <- id
  data$ResName <- name
  data$ResCreatedDate <- created_date
  data$ResModifiedDate <- modified_date

  # Reorder to move context columns to the front
  context_cols <- c("ResID", "ResName", "ResCreatedDate", "ResModifiedDate")
  all_cols <- names(data)
  other_cols <- all_cols[!(all_cols %in% context_cols)]

  return(data[, c(context_cols, other_cols)])
}
