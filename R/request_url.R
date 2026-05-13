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

        # Parse each part, splitting only on the first "="
        query_list <- list()
        for (part in parts) {
          pos <- regexpr("=", part, fixed = TRUE)
          if (pos > 0) {
            name <- substr(part, 1, pos - 1)
            value <- substr(part, pos + 1, nchar(part))
            query_list[[name]] <- value
          } else {
            query_list[[part]] <- ""
          }
        }

        req <- req |> httr2::req_url_query(!!!query_list)
      }
    }
  }

  return(req)
}
