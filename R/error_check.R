#' Throws errors found in the content of an `httr::GET` request
#'
#' @param content object produced by `httr::content`
#' @keywords internal
#' @noRd
error_check <- function(content, call = rlang::caller_env()) {
  # if content is not a list (and not an xml_document),
  # stop for content (a string describing an error)
  if (!is.list(content) && !inherits(content, "xml_document")) {
    rlang::abort(
      paste("API error:", content),
      call = call
    )
  }

  if (inherits(content, "xml_document")) {
    rlang::abort(
      "Can't find resource with ID. The API returned an HTML page instead of JSON. This often happens for 404 or 500 errors.",
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
