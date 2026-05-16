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
  # Handle NULL modified_date
  if (is.null(modified_date)) {
    modified_date <- NA_character_
  }

  # Vectorized date parsing
  created_date <- as.POSIXct(created_date, format = "%FT%X", tz = "UTC")
  modified_date <- as.POSIXct(modified_date, format = "%FT%X", tz = "UTC")

  # Vectorized identity correction (ensures created <= modified) using purrr
  # Actually, logical indexing is already vectorized and efficient.
  # But we can use purrr if preferred for consistency.
  # For simple vector logic, standard R indexing is usually better,
  # but here's a purrr-flavored alternative for the correction:
  m_lt_c <- !is.na(modified_date) & modified_date < created_date
  modified_date[m_lt_c] <- created_date[m_lt_c]

  # Efficient prepending of context columns
  dplyr::mutate(
    data,
    ResID = id,
    ResName = name,
    ResCreatedDate = created_date,
    ResModifiedDate = modified_date,
    .before = dplyr::everything()
  )
}
