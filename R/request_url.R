#' Produces a URL for a GET request to opendata.nhs.scot
#'
#' @param action The API endpoint you want to use, e.g., "package_show" / "datastore_search".
#' @param query The query to pass to the endpoint defined by the action argument.
#' @return a URL as a character string
#' @keywords internal
#' @noRd
request_url <- function(action, query, call = rlang::caller_env()) {
  # If action is already a URL, return it
  if (grepl("^http", action)) {
    return(action)
  }

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
    # use manual concatenation to avoid encoding '=' in the path,
    # which matches expected behavior in tests.
    url <- paste0(base_url, "/datastore/dump/", query, "?bom=true")
  } else {
    # Build URL with httr2.
    req <- httr2::request(base_url) %>%
      httr2::req_url_path("api/3/action") %>%
      httr2::req_url_path_append(action)

    if (is.list(query)) {
      # Remove NULLs and add to query
      query <- query[!vapply(query, is.null, logical(1))]
      if (length(query) > 0) {
        req <- do.call(httr2::req_url_query, c(list(req), query))
      }
    } else if (is.character(query) && nchar(query) > 0) {
      if (grepl("=", query)) {
        # If it's a raw query string like "id=doop", append it manually
        # to avoid double-encoding issues.
        url_so_far <- req$url
        sep <- if (grepl("\\?", url_so_far)) "&" else "?"
        return(paste0(url_so_far, sep, query))
      } else {
        req <- httr2::req_url_query(req, id = query)
      }
    }
    url <- req$url
  }

  # return standard API endpoint (i.e., not dump)
  return(url)
}
