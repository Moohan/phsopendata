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
  # Vectorized comparison to handle both scalar and vector inputs
  invalid_mod <- !is.na(created_date) & !is.na(modified_date) &
    modified_date < created_date

  if (any(invalid_mod)) {
    modified_date[invalid_mod] <- created_date[invalid_mod]
  }

  # Use base R for faster column addition and reordering.
  # Prepend context columns by removing them first if they exist
  # (to mimic dplyr::mutate behavior) and then using cbind.
  target_cols <- c("ResID", "ResName", "ResCreatedDate", "ResModifiedDate")
  data <- data[, setdiff(names(data), target_cols), drop = FALSE]

  # Using a tibble here to avoid class mismatch issues and maintain tidyverse
  # expectations for the return type.
  context_df <- tibble::tibble(
    ResID = id,
    ResName = name,
    ResCreatedDate = created_date,
    ResModifiedDate = modified_date
  )

  # bind_cols handles recycling and maintains the tibble class
  data_with_context <- dplyr::bind_cols(context_df, data)

  return(data_with_context)
}
