#' Throws errors found in the content of an API request
#'
#' @param content object produced by the API
#' @keywords internal
#' @noRd
error_check <- function(content, call = rlang::caller_env()) {
  # if content is not a list,
  # stop for content (a string describing an error)
  if (!is.list(content)) {
    if (inherits(content, "xml_document")) {
      if (requireNamespace("xml2", quietly = TRUE)) {
        text <- xml2::xml_text(content)
        text <- gsub("\\s+", " ", text)
        text <- substr(trimws(text), 1, 500)
        cli::cli_abort(
          c(
            "API error (HTML response).",
            x = text
          ),
          call = call
        )
      } else {
        cli::cli_abort("API error (HTML response).", call = call)
      }
    }

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
