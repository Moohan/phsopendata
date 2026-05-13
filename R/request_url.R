#' Produces a request object for opendata.nhs.scot
#'
#' @param action The API endpoint you want to use, e.g., "package_show" / "datastore_search".
#' @param query The query to pass to the endpoint defined by the action argument.
#' @return An \code{httr2_request} object.
#' @keywords internal
#' @noRd
request_url <- function(action, query) {
  base_url <- "https://www.opendata.nhs.scot"

  if (action == "dump") {
    # return dump request
    # For dump, query is the resource ID (character string)
    full_url <- paste0(base_url, "/datastore/dump/", query, "?bom=true")
    return(httr2::request(full_url))
  }

  req <- httr2::request(base_url) |>
    httr2::req_url_path_append("api/3/action") |>
    httr2::req_url_path_append(action)

  if (length(query) > 0) {
    if (is.list(query)) {
      req <- req |> httr2::req_url_query(!!!query)
    } else if (is.character(query)) {
      # Parse query string, splitting only on first "=" per part
      parts <- strsplit(query, "&")[[1]]
      query_list <- list()
      for (part in parts) {
        pos <- regexpr("=", part, fixed = TRUE)
        if (pos > 0) {
          query_list[[substr(part, 1, pos - 1)]] <- substr(part, pos + 1, nchar(part))
        } else {
          query_list[[part]] <- ""
        }
      }
      req <- req |> httr2::req_url_query(!!!query_list)
    }
  }

  return(req)
}
