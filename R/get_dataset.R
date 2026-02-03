#' Get Open Data resources from a dataset
#'
#' @description Downloads multiple resources from a dataset on the NHS Open Data platform by dataset name, with optional row limits and context columns.
#'
#' @param dataset_name Name of the dataset as found on \href{https://www.opendata.nhs.scot/}{NHS Open Data platform} (character).
#' @param max_resources (optional) The maximum number of resources to return (integer). If not set, all resources are returned.
#' @inheritParams get_resource
#'
#' @seealso [get_resource()] for downloading a single resource from a dataset.
#'
#' @return A [tibble][tibble::tibble-package] with the data.
#' @export
#'
#' @examples
#' get_dataset("annual-outpatient-activity", max_resources = 2, rows = 10)
get_dataset <- function(
  dataset_name,
  max_resources = NULL,
  rows = NULL,
  row_filters = NULL,
  col_select = NULL,
  include_context = FALSE
) {
  # throw error if name type/format is invalid
  check_dataset_name(dataset_name)

  # define query and try API call
  query <- list("id" = dataset_name)
  content <- try(
    phs_GET("package_show", query),
    silent = TRUE
  )

  # if content contains a 'Not Found' error
  # throw error with suggested dataset name
  if (inherits(content, "try-error")) {
    if (grepl("Not Found", content[1], fixed = TRUE)) {
      suggest_dataset_name(dataset_name)
    } else {
      # if some other error occurred, stop now
      stop(content)
    }
  }

  # define list of resource IDs to get
  all_resources <- content$result$resources
  all_ids <- vapply(all_resources, function(x) x$id, character(1))

  n_res <- length(all_ids)
  res_index <- 1:min(n_res, max_resources)

  selection_ids <- all_ids[res_index]

  # get all resources
  all_data <- purrr::map(
    selection_ids,
    get_resource,
    rows = rows,
    row_filters = row_filters,
    col_select = col_select,
  )

  # resolve class issues
  # Use class()[1] for robustness and vapply for speed/type-safety
  types_list <- lapply(
    all_data,
    function(df) vapply(df, function(col) class(col)[1], character(1))
  )

  # Flatten all type vectors into one named vector
  all_types <- do.call(c, unname(types_list))

  if (length(all_types) > 0) {
    # Split type names into groups and check for inconsistencies
    split_types <- split(all_types, names(all_types))
    inconsistent <- vapply(
      split_types,
      function(x) length(unique(x)) > 1,
      logical(1)
    )
    to_coerce <- names(inconsistent)[inconsistent]
  } else {
    to_coerce <- character(0)
  }

  if (length(to_coerce) > 0) {
    cli::cli_warn(c(
      "Due to conflicts between column types across resources,
      the following {cli::qty(to_coerce)} column{?s} ha{?s/ve} been coerced to type character:",
      "{.val {to_coerce}}"
    ))

    all_data <- lapply(all_data, function(df) {
      cols_to_fix <- intersect(to_coerce, names(df))
      if (length(cols_to_fix) > 0) {
        df[cols_to_fix] <- lapply(df[cols_to_fix], as.character)
      }
      df
    })
  }

  if (include_context) {
    # Add the 'resource context' as columns to the data
    resources <- all_resources[res_index]
    all_data <- purrr::pmap(
      list(
        "data" = all_data,
        "id" = selection_ids,
        "name" = vapply(resources, function(x) x$name, character(1)),
        "created_date" = vapply(resources, function(x) x$created, character(1)),
        "modified_date" = vapply(
          resources,
          function(x) {
            m <- x$last_modified
            if (is.null(m)) NA_character_ else m
          },
          character(1)
        )
      ),
      add_context
    )
  }

  # Combine the list of resources into a single tibble
  combined <- purrr::list_rbind(all_data)

  return(combined)
}
