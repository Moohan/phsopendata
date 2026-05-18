#' Throws errors found in the content of an `httr2 request` request
#'
#' @param content object produced by `httr2 response`
#' @keywords internal
#' @noRd
error_check <- function(content, call = rlang::caller_env()) {
  # if content is an xml document, it indicates an HTML error page
  if (inherits(content, "xml_node")) {
    # Extract text from the HTML body if possible
    body_text <- xml2::xml_text(xml2::xml_find_first(content, "//body"))

    # Standardize the error message for non-existent IDs to match existing tests
    if (grepl("Not Found", body_text, ignore.case = TRUE)) {
      cli::cli_abort(
        c(
          "API error.",
          x = "Can't find resource with ID."
        ),
        call = call
      )
    }

    cli::cli_abort(
      c(
        "The API returned an HTML error page.",
        i = "This often indicates a server error or an invalid resource ID.",
        x = "HTML Body Snippet: {substr(trimws(body_text), 1, 100)}"
      ),
      call = call
    )
  }

  # if content is not a list,
  # stop for content (a string describing an error)
  if (!is.list(content) && !is.null(content)) {
    cli::cli_abort(
      c(
        "The API returned an unexpected response format.",
        x = "Expected a list (JSON), but received: {.type {content}}"
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
