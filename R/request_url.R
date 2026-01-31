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
    # Return dump URL. We use paste0 here to match legacy httr behavior
    # of not escaping characters like '=' in the resource ID path component.
    return(paste0(base_url, "/datastore/dump/", query, "?bom=true"))
  }

  req <- httr2::request(base_url) %>%
    httr2::req_url_path("api", "3", "action", action)

  if (is.list(query)) {
    # Remove NULLs to avoid errors in httr2
    query <- query[!vapply(query, is.null, logical(1))]
    req <- req %>% httr2::req_url_query(!!!query)
  } else if (is.character(query) && nchar(query) > 0) {
    # Handle raw query strings by appending to the URL to match legacy httr behavior
    return(paste0(req$url, "?", query))
  }

  # return standard API endpoint (i.e., not dump)
  return(req$url)
}
