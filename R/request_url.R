#' Produces a URL for a GET request to opendata.nhs.scot
#'
#' @param action The API endpoint you want to use, e.g., "package_show" /
#' "datastore_search".
#' @param query The query to pass to the endpoint defined by the action
#' argument.
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
    # Manual construction for dump to maintain CKAN compatibility with '='
    url <- paste0(base_url, "/datastore/dump/", query, "?bom=true")
  } else {
    url_obj <- httr2::url_parse(base_url)
    url_obj$path <- paste0("api/3/action/", action)

    # Handle string queries (legacy support) vs named lists
    if (is.character(query) && length(query) == 1L && grepl("=", query)) {
      # Manual construction to avoid httr2 query validation on strings
      url <- paste0(base_url, "/api/3/action/", action, "?", query)
    } else {
      url_obj$query <- query
      url <- httr2::url_build(url_obj)
    }
  }

  url
}
