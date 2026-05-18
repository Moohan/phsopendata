#' Get the latest resource from a dataset
#'
#' @description
#' The Scottish Health and Social Care Open Data platform
#' hosts some datasets that are updated regularly with new resources.
#' For some of these datasets, the latest resource is the one that
#' is most likely to be of interest. This function attempts to
#' identify the latest resource for a given dataset and returns
#' it as a tibble.
#'
#' @inheritParams get_dataset
#' @param include_context (optional) If `TRUE`, additional information about the
#' resource will be added as columns to the data. Defaults to `TRUE` for this
#' function.
#'
#' @seealso [get_resource()] for downloading a single resource from a dataset.
#' @seealso [get_dataset()] for downloading all resources from a dataset.
#'
#' @return A [tibble][tibble::tibble-package] with the data.
#' @export
#'
#' @examplesIf isTRUE(length(curl::nslookup("www.opendata.nhs.scot", error =
#' FALSE)) > 0L)
#' \dontrun{
#' get_latest_resource("gp-practice-populations", rows = 10)
#' }
get_latest_resource <- function(dataset_name,
                                rows = NULL,
                                row_filters = NULL,
                                col_select = NULL,
                                include_context = TRUE) {
  # define the applicable datasets
  applicable_datasets <- c(
    "gp-practice-populations",
    "quality-outcome-framework-indicators",
    "child-and-adolescent-mental-health-waiting-times",
    "child-and-adolescent-mental-health-waiting-times-adhoc",
    "psychological-therapies-waiting-times"
  )

  # check if the dataset is within the applicable datasets
  if (!dataset_name %in% applicable_datasets) {
    cli::cli_abort(
      c(
        paste(
          "The dataset name supplied {.val {dataset_name}} is not within the",
          "applicable datasets."
        ),
        i = "These are: {.val {applicable_datasets}}",
        x = "Please see {.fun get_latest_resource} documentation.",
        "*" = paste(
          "You can find dataset names in the URL of a dataset's page on",
          "{.url www.opendata.nhs.scot}."
        )
      )
    )
  }

  # get the latest resource id
  res_id <- get_latest_resource_id(dataset_name)

  # get the resource
  get_resource(
    res_id = res_id,
    rows = rows,
    row_filters = row_filters,
    col_select = col_select,
    include_context = include_context
  )
}
