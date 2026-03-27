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
  # Scalar or vector inputs are supported.

  # Parse dates if they are character vectors
  if (is.character(created_date)) {
    created_date <- as.POSIXct(created_date, format = "%FT%X", tz = "UTC")
  }

  if (is.null(modified_date)) {
    modified_date <- NA_character_
  }

  if (is.character(modified_date)) {
    modified_date <- as.POSIXct(modified_date, format = "%FT%X", tz = "UTC")
  }

  # Ensure created_date is always <= modified_date
  # Correction should ideally happen before expansion, but kept here for robustness
  m_lt_c <- !is.na(modified_date) & !is.na(created_date) & modified_date < created_date
  if (any(m_lt_c)) {
    modified_date[m_lt_c] <- created_date[m_lt_c]
  }

  # Vectorize metadata to match row count
  n_rows <- nrow(data)
  if (length(id) == 1L && n_rows > 1L) id <- rep(id, n_rows)
  if (length(name) == 1L && n_rows > 1L) name <- rep(name, n_rows)
  if (length(created_date) == 1L && n_rows > 1L) created_date <- rep(created_date, n_rows)
  if (length(modified_date) == 1L && n_rows > 1L) modified_date <- rep(modified_date, n_rows)

  # Column names for context
  target_cols <- c("ResID", "ResName", "ResCreatedDate", "ResModifiedDate")

  # Use base R to remove existing context columns and prepend new ones
  # This is much faster than dplyr::mutate(.before = everything())
  data <- data[, setdiff(names(data), target_cols), drop = FALSE]

  data_with_context <- dplyr::bind_cols(
    tibble::tibble(
      ResID = id,
      ResName = name,
      ResCreatedDate = created_date,
      ResModifiedDate = modified_date
    ),
    data
  )

  return(data_with_context)
}
