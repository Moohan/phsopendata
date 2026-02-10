#' Parse CKAN error details into a structured format
#'
#' @param error The "error" element of an object produced by `httr::content`.
#' @return A list with a `message` string and optionally a `bullets` character vector.
#' @noRd
#' @keywords internal
parse_ckan_error_details <- function(error) {
  error_message <- error$message
  error_type <- error$`__type`

  # default message
  message <- paste0(error_type, ": ", error_message)

  bullets <- NULL

  # special case for validation errors
  if (error_type == "Validation Error") {
    # remove __type and message from the error list to only have field errors
    field_errors <- error[!(names(error) %in% c("__type", "message"))]

    if (length(field_errors) > 0) {
      message <- "Validation Error"
      bullets <- purrr::imap_chr(field_errors, function(err, name) {
        # Translate names for package users
        name <- sub("fields", "col_select", name, fixed = TRUE)
        name <- sub("q", "row_filters", name, fixed = TRUE)
        paste0(name, ": ", paste(unlist(err), collapse = ", "))
      })
      names(bullets) <- rep("*", length(bullets))
    }
  }

  # special case for SQL validation errors
  if (!is.null(error$info$orig)) {
    message <- sub("^", "", error$info$orig[[1]], fixed = TRUE)
    message <- sub("LINE 1:", "In SQL:", message, fixed = TRUE)
    message <- sub(
      "relation",
      "resource/table",
      message,
      fixed = TRUE
    )
  }

  return(list(message = message, bullets = bullets))
}
