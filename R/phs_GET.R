#' Send a GET request to the PHS CKAN API
#'
#' @inheritParams request_url
#' @param verbose TRUE or FALSE. If TRUE, a success message will be printed to
#' the console.
#' @return content of a httr2 request request
#' @keywords internal
#' @noRd
phs_GET <- function(action,
                    query,
                    verbose = FALSE,
                    call = rlang::caller_env()) {
  # define URL
  url <- request_url(action, query)

  # Attempt GET request, gently retrying up to 3 times
  # Use req_error(is_error = ~ FALSE) to handle errors manually via error_check
  response <- tryCatch(
    httr2::request(url) |>
      httr2::req_user_agent(
        "phsopendata (https://github.com/Public-Health-Scotland/phsopendata)"
      ) |>
      httr2::req_retry(max_tries = 3) |>
      httr2::req_error(is_error = ~ FALSE) |>
      httr2::req_perform(),
    error = function(e) {
      cli::cli_abort(
        c(
          "Can't connect to the CKAN server.",
          i = "Check your network or proxy settings.",
          "x" = e$message
        ),
        call = call
      )
    }
  )

  # Extract the content from the HTTP response
  content_type <- httr2::resp_content_type(response)

  if (content_type %in% c("text/html", "application/json")) {
    if (content_type == "text/html") {
      # Handle HTML error pages or redirects
      content <- xml2::read_html(httr2::resp_body_string(response))
    } else {
      # Use simplifyVector = FALSE for compatibility with existing record parsing
      content <- httr2::resp_body_json(response, simplifyVector = FALSE)
    }
  } else if (content_type == "text/csv") {
    content <- readr::read_csv(
      file = I(httr2::resp_body_string(response)),
      guess_max = Inf
    )
  } else {
    cli::cli_abort(paste0(
      "The response contained an unhandled content type: ",
      "{content_type}"
    ))
  }

  # detect/handle errors
  error_check(content, call = call)

  if (verbose) cat("GET request successful.\n")
  content
}
