#' Use datastore dump to download whole files
#'
#' @param res_id a resource ID
#' @return dataframe containing resource records
#' @keywords internal
#' @noRd
dump_download <- function(res_id, call = rlang::caller_env()) {
  # fetch the data
  content <- phs_GET("dump", res_id)

  # return data
  return(content[, -1L])
}
