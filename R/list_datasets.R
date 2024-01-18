#' List dataset names
#'
#' @return a character vector of dataset names
#' @export
#'
#' @examples list_datasets()
list_datasets <- function() {
  content <- phs_GET(action = "package_list", query = NULL)
  dataset_names <- unlist(content$result)

  return(dataset_names)
}
