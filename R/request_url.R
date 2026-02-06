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
    # Use manual construction to match existing behavior and avoid unwanted encoding in path
    url <- paste0(base_url, "/datastore/dump/", query, "?bom=true")
  } else {
    url_obj <- httr2::url_parse(base_url)
    url_obj$path <- paste(c("api/3/action", action), collapse = "/")

    if (is.character(query)) {
      # If query is already a string, append it manually
      url <- httr2::url_build(url_obj)
      if (any(nzchar(query))) {
        url <- paste0(url, "?", query)
      }
    } else {
      # Remove NULLs from query to ensure clean URL construction
      url_obj$query <- query[!vapply(query, is.null, logical(1))]
      url <- httr2::url_build(url_obj)
    }
  }

  # return standard API endpoint (i.e., not dump)
  return(url)
}
