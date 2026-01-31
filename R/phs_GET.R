#' Send a GET request to the PHS CKAN API
#'
#' @inheritParams request_url
#' @param verbose TRUE or FALSE. If TRUE, a success message will be printed to the console.
#' @return content of a httr2::request
#' @keywords internal
#' @noRd
phs_GET <- function(
  action,
  query,
  verbose = FALSE,
  call = rlang::caller_env()
) {
  # define URL
  url <- request_url(action, query)

  # Build request
  req <- httr2::request(url) %>%
    httr2::req_user_agent("phsopendata (https://github.com/Public-Health-Scotland/phsopendata)") %>%
    httr2::req_retry(max_tries = 4) %>%
    # Don't throw on HTTP errors (e.g. 404, 409) so we can handle them
    # in error_check() by parsing the response body.
    httr2::req_error(is_error = ~FALSE)

  # Perform the request
  response <- tryCatch(
    httr2::req_perform(req),
    httr2_failure = function(cnd) {
      cli::cli_abort(
        c(
          "Can't connect to the CKAN server.",
          i = "Check your network/proxy settings."
        ),
        call = call,
        parent = cnd
      )
    }
  )

  # Extract the content from the HTTP response
  content_type <- httr2::resp_content_type(response)

  if (grepl("application/json", content_type, fixed = TRUE)) {
    content <- httr2::resp_body_json(response)
  } else if (grepl("text/html", content_type, fixed = TRUE)) {
    # The API sometimes returns JSON with a text/html content type.
    # If it's actually HTML, resp_body_json will fail.
    content <- try(
      httr2::resp_body_json(response, check_type = FALSE),
      silent = TRUE
    )
    if (inherits(content, "try-error")) {
      # If it's not JSON, it might be a real HTML page (e.g. 404 for dump).
      # If xml2 is available, we return an xml_document to match legacy behavior.
      if (requireNamespace("xml2", quietly = TRUE)) {
        content <- httr2::resp_body_html(response)
      } else {
        status <- httr2::resp_status(response)
        if (status == 404) {
          content <- "Not Found Error"
        } else {
          content <- paste("HTTP error", status)
        }
      }
    }
  } else if (grepl("text/csv", content_type, fixed = TRUE)) {
    content <- readr::read_csv(
      file = httr2::resp_body_string(response),
      guess_max = Inf
    )
  } else {
    cli::cli_abort(
      "The response contained an unhandled content type: {content_type}"
    )
  }

  # detect/handle errors
  error_check(content, call = call)

  if (verbose) cat("GET request successful.\n")
  return(content)
}
