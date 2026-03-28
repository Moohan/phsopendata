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
  # created_date is always <= modified_date
  m_lt_c <- !is.na(modified_date) & modified_date < created_date
  if (any(m_lt_c)) {
    modified_date[m_lt_c] <- created_date[m_lt_c]
  }

  # Add context columns and move them to the front
  data[["ResID"]] <- id
  data[["ResName"]] <- name
  data[["ResCreatedDate"]] <- created_date
  data[["ResModifiedDate"]] <- modified_date

  # Reorder to move new columns to front.
  # Using match() or integer subsetting is faster than dplyr::relocate for large dfs.
  n_cols <- ncol(data)
  context_cols <- (n_cols - 3L):n_cols
  data <- data[, c(context_cols, setdiff(seq_len(n_cols), context_cols)), drop = FALSE]

  return(data)
}
