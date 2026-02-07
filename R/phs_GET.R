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
    httr2::req_error(is_error = function(resp) FALSE)

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
    # The API sometimes returns JSON with a text/html content type
    content <- tryCatch(
      httr2::resp_body_json(response, check_type = FALSE),
      error = function(e) {
        # If it really is HTML, return it as an xml_document if xml2 is available
        if (requireNamespace("xml2", quietly = TRUE)) {
          xml2::read_html(httr2::resp_body_string(response))
        } else {
          # Fallback to status description
          paste("HTTP", httr2::resp_status(response), httr2::resp_status_desc(response))
        }
      }
    )
  } else if (grepl("text/csv", content_type, fixed = TRUE)) {
    content <- readr::read_csv(
      file = I(httr2::resp_body_string(response)),
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
