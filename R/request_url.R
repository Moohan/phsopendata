#' Produces a request object for opendata.nhs.scot
#'
#' @param action The API endpoint you want to use, e.g., "package_show" / "datastore_search".
#' @param query The query to pass to the endpoint defined by the action argument.
#' @return An \code{httr2_request} object.
#' @keywords internal
#' @noRd
request_url <- function(action, query, call = rlang::caller_env()) {
  # check action is valid
  valid_actions <- c(
    "datastore_search",
    "datastore_search_sql",
    "dump",
    "package_show",
    "package_list",
    "resource_show",
    "package_search"
  )
  if (!(action %in% valid_actions)) {
    cli::cli_abort(
      c(
        "API call failed.",
        x = "{.val {action}} is an invalid {.arg action} argument."
      ),
      call = call
    )
  }

  base_url <- "https://www.opendata.nhs.scot"

  if (action == "dump") {
    # return dump request
    # For dump, query is the resource ID (character string)
    # We must avoid URL-encoding = in the path (e.g., id=doop becoming id%3Ddoop).
    # Since httr2::request() may still escape the path even if passed a full URL,
    # we use httr2::req_url() to manually set the URL and then bypass escaping if possible.
    # However, httr2 always tries to maintain a valid URL.
    # A safer way to bypass escaping in the path is not directly supported in httr2's higher level API.
    # We will use req_url() on a base request.
    full_url <- paste0(base_url, "/datastore/dump/", query, "?bom=true")
    req <- httr2::request(base_url) |>
      httr2::req_url(full_url)
  } else {
    req <- httr2::request(base_url) |>
      httr2::req_url_path_append("api/3/action") |>
      httr2::req_url_path_append(action)

    if (length(query) > 0) {
      if (is.list(query)) {
        req <- req |> httr2::req_url_query(!!!query)
      } else if (is.character(query) && length(query) == 1 && nzchar(query)) {
        # Split query string into components to avoid escaping issues with req_url_query
        parts <- strsplit(query, "&")[[1]]
        kv <- strsplit(parts, "=")
        query_list <- stats::setNames(
          lapply(kv, function(x) if(length(x) > 1) x[2] else ""),
          vapply(kv, function(x) x[1], character(1))
        )
        req <- req |> httr2::req_url_query(!!!query_list)
      }
    }
  }

  return(req)
}
