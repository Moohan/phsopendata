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

  # Parse the date values
  # Optimized: Use explicit format string for robustness
  created_date <- as.POSIXct(created_date, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
  modified_date <- as.POSIXct(modified_date, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")

  # The platform can record the modified date as being before the created date
  # by a few microseconds, this will catch any rounding which ensure
  # created_date is always <= modified_date
  # Optimized: Vectorized robust logical comparison
  inconsistent_dates <- !is.na(created_date) & !is.na(modified_date) &
    modified_date < created_date

  if (any(inconsistent_dates)) {
    modified_date[inconsistent_dates] <- created_date[inconsistent_dates]
  }

  # Optimized: Using bind_cols is faster than mutate for prepending columns.
  # We use rep() to ensure it works correctly for 0-row data frames and
  # supports both scalar and vector inputs for context fields.
  n_rows <- nrow(data)
  context_data <- tibble::tibble(
    ResID = rep(id, length.out = n_rows),
    ResName = rep(name, length.out = n_rows),
    ResCreatedDate = rep(created_date, length.out = n_rows),
    ResModifiedDate = rep(modified_date, length.out = n_rows)
  )

  # Optimized: Mimic mutate's overwrite behavior by removing target columns first
  data <- data[, setdiff(names(data), names(context_data)), drop = FALSE]

  data_with_context <- dplyr::bind_cols(context_data, data)

  return(data_with_context)
}
