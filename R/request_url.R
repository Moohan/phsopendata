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
  req <- httr2::request(base_url)

  if (action == "dump") {
    # return dump request
    # For dump, query is the resource ID (character string)
    req <- req |>
      httr2::req_url_path_append("datastore/dump") |>
      httr2::req_url_path_append(query) |>
      httr2::req_url_query(bom = "true")
  } else {
    # query is a named list
    req <- req |>
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
