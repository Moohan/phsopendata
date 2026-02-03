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

  # Parse the base URL
  url_obj <- httr2::url_parse(base_url)

  # Handle empty query strings that some internal functions pass
  if (identical(query, "")) {
    query <- list()
  }

  if (action == "dump") {
    # return dump URL
    url_obj$path <- paste(c("datastore/dump", query), collapse = "/")
    url_obj$query <- list(bom = "true")
  } else {
    # return standard API endpoint URL
    url_obj$path <- paste(c("api/3/action", action), collapse = "/")
    url_obj$query <- query
  }

  return(httr2::url_build(url_obj))
}
