#' Send a GET request to the PHS CKAN API
#'
#' @inheritParams request_url
#' @param verbose TRUE or FALSE. If TRUE, a success message will be printed to the console.
#' @return content of a request
#' @keywords internal
#' @noRd
phs_GET <- function(
  action,
  query,
  verbose = FALSE,
  call = rlang::caller_env()
) {
  # define request
  req <- request_url(action, query)

  # Configure request:
  # - Set user agent
  # - Retry up to 3 times for transient errors (429, 503, etc.)
  # - Custom error handling to extract information from CKAN error bodies
  req <- req |>
    httr2::req_user_agent(
      "phsopendata (https://github.com/Public-Health-Scotland/phsopendata)"
    ) |>
    httr2::req_retry(max_tries = 3) |>
    httr2::req_error(
      body = function(resp) {
        tryCatch(
          {
            # CKAN often returns error details in JSON body even for 4xx/5xx
            if (httr2::resp_has_body(resp) &&
              httr2::resp_content_type(resp) == "application/json") {
              body <- httr2::resp_body_json(resp, simplifyVector = FALSE)
              if (!is.null(body$error)) {
                return(parse_error(body$error))
              }
            }
            return(character())
          },
          error = function(e) character()
        )
      }
    )

  # Attempt GET request
  response <- tryCatch(
    httr2::req_perform(req),
    error = function(cnd) {
      if (inherits(cnd, "httr2_error_request")) {
        cli::cli_abort(
          c(
            "Can't connect to the CKAN server.",
            i = "Check your network or proxy settings."
          ),
          parent = cnd,
          call = call
        )
      }
      # Re-throw other errors (including httr2_error_status which are handled by req_error above)
      rlang::cnd_signal(cnd)
    }
  )

  # Extract the content from the HTTP response
  type <- httr2::resp_content_type(response)

  if (type %in% c("text/html", "application/json")) {
    content <- httr2::resp_body_json(response, simplifyVector = FALSE)
  } else if (type == "text/csv") {
    content <- readr::read_csv(
      file = I(httr2::resp_body_string(response)),
      guess_max = Inf,
      show_col_types = FALSE
    )
  } else {
    cli::cli_abort(
      "The response contained an unhandled content type: {type}",
      call = call
    )
  }

  # detect/handle errors (specifically for CKAN 200 OK but success=FALSE)
  error_check(content, call = call)

  if (verbose) cat("GET request successful.\n")
  return(content)
}
