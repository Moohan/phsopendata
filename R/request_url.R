#' Produces a URL for a GET request to opendata.nhs.scot
#'
#' @param action The API endpoint you want to use, e.g., "package_show" / "datastore_search".
#' @param query The query to pass to the endpoint defined by the action argument.
#' @return a URL as a character string
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
    "resource_show"
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
    # return dump URL
    # We build the URL manually to match httr::modify_url behavior for this endpoint
    # specifically to avoid escaping of '=' if present in query/id.
    url <- paste0(base_url, "/datastore/dump/", query, "?bom=true")
  } else {
    req <- httr2::request(base_url)
    req <- httr2::req_url_path(req, "api", "3", "action", action)

    if (is.list(query)) {
      req <- httr2::req_url_query(req, !!!query)
      url <- req$url
    } else if (is.character(query) && length(query) == 1 && nchar(query) > 0) {
      url <- paste0(req$url, "?", query)
    } else {
      url <- req$url
    }
  }

  # return standard API endpoint (i.e., not dump)
  return(url)
}
