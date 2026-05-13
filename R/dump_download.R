#' Use datastore dump to download whole files
#'
#' @param res_id a resource ID
#' @return dataframe containing resource records
#' @keywords internal
#' @noRd
dump_download <- function(res_id, call = rlang::caller_env()) {
  # fetch the data
  # phs_GET will now throw 404 for non-existent res_ids
  content <- tryCatch(
    suppressMessages(phs_GET("dump", res_id)),
    httr2_http_404 = function(cnd) {
      cli::cli_abort(
        "Can't find resource with ID {.var {res_id}} in datastore.",
        parent = cnd,
        call = call
      )
    }
  )

  # if content is a web page (though httr2 should have handled 404 above)
  if (inherits(content, "xml_document")) {
    cli::cli_abort(
      "Can't find resource with ID {.var {res_id}} in datastore.",
      call = call
    )
  }

  # return data
  return(content[, -1L])
}
