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
  # Parse the date values if they aren't already POSIXct
  if (!inherits(created_date, "POSIXct")) {
    created_date <- as.POSIXct(created_date, format = "%FT%X", tz = "UTC")
  }

  if (!inherits(modified_date, "POSIXct")) {
    # Catch if the resource has never been modified
    if (is.null(modified_date)) {
      modified_date <- NA_character_
    }
    modified_date <- as.POSIXct(modified_date, format = "%FT%X", tz = "UTC")
  }

  # The platform can record the modified date as being before the created date
  # by a few microseconds, this will catch any rounding which ensure
  # created_date is always <= modified_date
  # Using dplyr::if_else for vectorization and NA safety
  modified_date <- dplyr::if_else(
    !is.na(modified_date) & modified_date < created_date,
    created_date,
    modified_date
  )

  # Robustly add context columns.
  # Using explicit column removal and bind_cols is faster than mutate()
  # for hot paths and handles 0-row data frames gracefully.
  target_cols <- c("ResID", "ResName", "ResCreatedDate", "ResModifiedDate")
  data <- data[, setdiff(names(data), target_cols), drop = FALSE]

  n_rows <- nrow(data)
  context <- tibble::tibble(
    ResID = rep(id, length.out = n_rows),
    ResName = rep(name, length.out = n_rows),
    ResCreatedDate = rep(created_date, length.out = n_rows),
    ResModifiedDate = rep(modified_date, length.out = n_rows)
  )

  data_with_context <- dplyr::bind_cols(context, data)

  return(data_with_context)
}
