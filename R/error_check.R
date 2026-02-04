#' Throws errors found in the content of an `httr2::GET` request
#'
#' @param content object produced by `httr2::content`
#' @keywords internal
#' @noRd
error_check <- function(content, call = rlang::caller_env()) {
  # if content is not a list,
  # stop for content (a string describing an error)
  if (!is.list(content)) {
    # For API error bodies that may contain HTML, avoid using cli::cli_abort
    # as it may misinterpret curly braces in HTML as glue expressions.
    rlang::abort(
      paste0("API error: ", content),
      call = call
    )
  }

  # if there is no error status/message in the content,
  # break out of the function
  is_error <- suppressWarnings(
    !is.null(content$error)
  )
  if (!is_error) {
    return()
  }

  # generate error message and stop
  error_text <- parse_error(content$error)
  cli::cli_abort(
    c(
      "API error.",
      x = error_text
    ),
    call = call
  )
}
