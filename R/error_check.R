#' Throws errors found in the content of an `httr2` request
#'
#' @param content object produced by `httr2::resp_body_json`
#' @keywords internal
#' @noRd
error_check <- function(content, call = rlang::caller_env()) {
  # if content is a web page, it's likely a 404 or other server error.
  if (inherits(content, "xml_document")) {
    cli::cli_abort(
      c(
        "Not Found Error: The requested resource or dataset was not found.",
        i = "The server returned an HTML error page."
      ),
      call = call
    )
  }

  # if content is not a list,
  # stop for content (a string describing an error)
  if (!is.list(content)) {
    cli::cli_abort(
      c(
        "API error",
        x = content
      ),
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
