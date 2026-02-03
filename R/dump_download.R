#' Use datastore dump to download whole files
#'
#' @param res_id a resource ID
#' @return dataframe containing resource records
#' @keywords internal
#' @noRd
dump_download <- function(res_id, call = rlang::caller_env()) {
  # fetch the data
  content <- tryCatch(
    suppressMessages(phs_GET("dump", res_id)),
    error = function(e) {
      if (grepl("HTML response", e$message)) {
        cli::cli_abort(
          c(
            "Can't find resource with ID {.var {res_id}} in datastore."
          ),
          call = call
        )
      }
      stop(e)
    }
  )

  # if content is a web page (redundant if phs_GET/error_check already aborts,
  # but good for safety if we changed something)
  if (inherits(content, "xml_document")) {
    cli::cli_abort(
      c(
        "Can't find resource with ID {.var {res_id}} in datastore."
      ),
      call = call
    )
  }

  # return data
  return(content[, -1])
}
