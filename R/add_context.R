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
  # Catch if the resource has never been modified
  if (is.null(modified_date)) {
    modified_date <- NA_character_
  }

  # Parse the date values if they aren't already POSIXct
  # Optimization: only parse if necessary to avoid overhead in vectorized calls
  if (!inherits(created_date, "POSIXct")) {
    created_date <- as.POSIXct(created_date, format = "%FT%X", tz = "UTC")
  }
  if (!inherits(modified_date, "POSIXct")) {
    modified_date <- as.POSIXct(modified_date, format = "%FT%X", tz = "UTC")
  }

  # The platform can record the modified date as being before the created date
  # by a few microseconds, this will catch any rounding which ensure
  # created_date is always <= modified_date
  # Vectorized comparison to handle multiple resources efficiently
  conflict <- !is.na(modified_date) & !is.na(created_date) & modified_date < created_date
  if (any(conflict)) {
    modified_date[conflict] <- created_date[conflict]
  }

  # Base R optimization: significantly faster than dplyr::mutate for adding columns
  # and avoids copy-on-modify overhead of multiple mutate steps.
  data$ResID <- id
  data$ResName <- name
  data$ResCreatedDate <- created_date
  data$ResModifiedDate <- modified_date

  # Reorder columns to put context first (mimics .before = everything())
  context_cols <- c("ResID", "ResName", "ResCreatedDate", "ResModifiedDate")
  data <- data[, c(context_cols, setdiff(names(data), context_cols))]

  return(data)
}
