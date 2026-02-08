#' Use datastore dump to download whole files
#'
#' @param res_id a resource ID
#' @return dataframe containing resource records
#' @keywords internal
#' @noRd
dump_download <- function(res_id, call = rlang::caller_env()) {
  # fetch the data
  content <- tryCatch(
    suppressMessages(
      phs_GET("dump", res_id)
    ),
    error = function(e) {
      # If not found, throw the expected dump error message
      if (grepl("Not Found Error", e$message)) {
        cli::cli_abort(
          c(
            "Can't find resource with ID {.var {res_id}} in datastore."
          ),
          call = call
        )
      }
      # Otherwise re-throw
      stop(e)
    }
  )

  # if content is a web page
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
