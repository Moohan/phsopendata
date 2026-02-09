#' Send a GET request to the PHS CKAN API
#'
#' @inheritParams request_url
#' @param verbose TRUE or FALSE. If TRUE, a success message will be printed to the console.
#' @return content of a httr::GET request
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

  # Attempt GET request, gently retrying up to 3 times
  response <- httr::RETRY(
    verb = "GET",
    url = url,
    terminate_on = 409L,
    user_agent = httr::user_agent(
      "phsopendata (https://github.com/Public-Health-Scotland/phsopendata)"
    )
  )

  # Check for a response from the server
  if (!inherits(response, "response")) {
    cli::cli_abort(
      c(
        "Can't connect to the CKAN server",
        i = "Check your network or proxy settings"
      ),
      call = call
    )
  }

  # Extract the content from the HTTP response
  if (httr::http_type(response) %in% c("text/html", "application/json")) {
    content <- httr::content(response)
  } else if (httr::http_type(response) == "text/csv") {
    content <- readr::read_csv(
      file = I(httr::content(response, as = "text")),
      guess_max = Inf,
      show_col_types = FALSE
    )
  } else {
    cli::cli_abort(
      "The response contained an unhandled content type: {.val {httr::http_type(response)}}",
      call = call
    )
  }

  # detect/handle errors
  is_ckan_error <- is.list(content) &&
    !inherits(content, "data.frame") &&
    identical(content$success, FALSE)

  if (httr::http_error(response) || is_ckan_error) {

    # Special case for dump 404
    if (action == "dump" && httr::status_code(response) == 404) {
      cli::cli_abort(
        "Can't find resource with ID {.val {query}} in datastore",
        class = c("phsopendata_error_not_found", "phsopendata_error"),
        call = call
      )
    }

    # If content is not a list, it's likely a raw error message (e.g. HTML)
    if (!is.list(content)) {
      cli::cli_abort(
        c(
          "API error",
          "x" = "{content}"
        ),
        call = call
      )
    }

    # If there is an error status/message in the content, parse it
    if (!is.null(content$error)) {
      error_details <- parse_error_details(content$error)

      error_type <- content$error$`__type`
      error_class <- switch(
        error_type,
        "Not Found Error" = "phsopendata_error_not_found",
        "Validation Error" = "phsopendata_error_validation",
        "Authorization Error" = "phsopendata_error_authorization",
        NULL
      )

      cli::cli_abort(
        c(
          "API error",
          "x" = "{error_details$message}",
          error_details$bullets
        ),
        class = c(error_class, "phsopendata_error"),
        call = call
      )
    } else {
      # Fallback to httr status if no CKAN error info is available
      httr::stop_for_status(response, task = "connect to the CKAN server")
    }
  }

  if (verbose) cat("GET request successful.\n")
  return(content)
}

#' Parse CKAN error details into a structured format
#'
#' @param error The "error" element of an object produced by `httr::content`.
#' @return A list with a `message` string and optionally a `bullets` character vector.
#' @noRd
#' @keywords internal
parse_error_details <- function(error) {
  error_message <- error$message
  error_type <- error$`__type`

  # default message
  message <- paste0(error_type, ": ", error_message)

  bullets <- NULL

  # special case for validation errors
  if (error_type == "Validation Error") {
    message <- paste0(names(error[1][1]), ": ", error[1][[1]])

    # translate message for package users
    message <- sub("fields", "col_select", message, fixed = TRUE)
    message <- sub("q", "row_filters", message, fixed = TRUE)
  }

  # special case for SQL validation errors
  if (!is.null(error$info$orig)) {
    message <- sub("\\^", "", error$info$orig[[1]], fixed = TRUE)
    message <- sub("LINE 1:", "In SQL:", message, fixed = TRUE)
    message <- sub(
      "relation",
      "resource/table",
      message,
      fixed = TRUE
    )
  }

  return(list(message = message, bullets = bullets))
}
