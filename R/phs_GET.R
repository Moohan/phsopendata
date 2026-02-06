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
    httr2::req_retry(
      max_tries = 5,
      is_transient = function(resp) {
        httr2::resp_status(resp) %in% c(429, 500, 502, 503, 504)
      }
    ) %>%
    # Don't throw on 4xx/5xx errors so we can handle them manually
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
    # API sometimes returns HTML bodies on error despite JSON content type
    content <- tryCatch(
      httr2::resp_body_json(response),
      error = function(e) httr2::resp_body_string(response)
    )
  } else if (grepl("text/html", content_type, fixed = TRUE)) {
    # The API sometimes returns JSON with a text/html content type
    content <- tryCatch(
      httr2::resp_body_json(response, check_type = FALSE),
      error = function(e) httr2::resp_body_string(response)
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
