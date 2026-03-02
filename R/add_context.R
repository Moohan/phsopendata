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
  # If dates are character, parse them.
  # If they are already POSIXct (vectorized case), this is skipped or handled safely.
  if (is.character(created_date)) {
    created_date <- as.POSIXct(created_date, format = "%FT%X", tz = "UTC")
  }

  if (is.character(modified_date)) {
    modified_date <- as.POSIXct(modified_date, format = "%FT%X", tz = "UTC")
  } else if (is.null(modified_date)) {
    modified_date <- as.POSIXct(NA_character_)
  }

  # The platform can record the modified date as being before the created date
  # by a few microseconds, this will catch any rounding which ensures
  # created_date is always <= modified_date.
  # Handles both scalar and vector inputs.
  idx_to_fix <- !is.na(modified_date) & !is.na(created_date) &
    modified_date < created_date

  if (any(idx_to_fix)) {
    modified_date[idx_to_fix] <- created_date[idx_to_fix]
  }

  # For performance in hot paths (like get_dataset), we use base R assignment
  # for context enrichment as it's significantly faster than dplyr::mutate.
  # We remove any existing context columns first to ensure we don't duplicate.
  target_cols <- c("ResID", "ResName", "ResCreatedDate", "ResModifiedDate")
  data <- data[, setdiff(names(data), target_cols), drop = FALSE]

  # Prepend columns by creating a new data frame and binding
  context_df <- tibble::tibble(
    ResID = id,
    ResName = name,
    ResCreatedDate = created_date,
    ResModifiedDate = modified_date
  )

  # If data has multiple rows and context is scalar, tibble handles recycling.
  # If data is large and context is vectorized, we avoid mutate's overhead.
  data_with_context <- dplyr::bind_cols(context_df, data)

  return(data_with_context)
}
