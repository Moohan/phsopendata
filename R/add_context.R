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

  # Parse the date values if they are still character
  if (is.character(created_date)) {
    created_date <- as.POSIXct(created_date, format = "%FT%X", tz = "UTC")
  }
  if (is.character(modified_date)) {
    modified_date <- as.POSIXct(modified_date, format = "%FT%X", tz = "UTC")
  }

  # The platform can record the modified date as being before the created date
  # by a few microseconds, this will catch any rounding which ensure
  # created_date is always <= modified_date.
  # Use logical indexing for robust vectorized comparison
  m_lt_c <- !is.na(modified_date) &
    !is.na(created_date) &
    modified_date < created_date
  modified_date[m_lt_c] <- created_date[m_lt_c]

  # For performance in hot paths, use base R for column assignment and reordering
  # especially since this function is now called for large combined data frames.
  target_cols <- c("ResID", "ResName", "ResCreatedDate", "ResModifiedDate")

  # Remove existing context columns if they exist to allow overwriting
  data <- data[, setdiff(names(data), target_cols), drop = FALSE]

  # Add columns at the beginning
  # rep() ensures scalar inputs work for 0-row or multi-row data frames
  n_rows <- nrow(data)
  data_with_context <- dplyr::bind_cols(
    tibble::tibble(
      ResID = rep(id, length.out = n_rows),
      ResName = rep(name, length.out = n_rows),
      ResCreatedDate = rep(created_date, length.out = n_rows),
      ResModifiedDate = rep(modified_date, length.out = n_rows)
    ),
    data
  )

  return(data_with_context)
}
