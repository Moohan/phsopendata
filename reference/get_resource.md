# Get Open Data resource

Get Open Data resource

## Usage

``` r
get_resource(
  res_id,
  rows = NULL,
  row_filters = NULL,
  col_select = NULL,
  include_context = FALSE
)
```

## Arguments

- res_id:

  The resource ID as found on [NHS Open Data
  platform](https://www.opendata.nhs.scot/)

- rows:

  (optional) specify the max number of rows to return.

- row_filters:

  (optional) a named list or vector that specifies values of
  columns/fields to keep. e.g. list(Date = 20220216, Sex = "Female").

- col_select:

  (optional) a character vector containing the names of desired
  columns/fields. e.g. c("Date", "Sex").

- include_context:

  (optional) If `TRUE` additional information about the resource will be
  added as columns to the data, including the resource ID, the resource
  name, the creation date and the last modified/updated date.

## Value

a [tibble](https://tibble.tidyverse.org/reference/tibble-package.html)
with the data

## See also

[`get_dataset()`](https://public-health-scotland.github.io/phsopendata/reference/get_dataset.md)
for downloading all resources from a given dataset.

## Examples

``` r
res_id <- "ca3f8e44-9a84-43d6-819c-a880b23bd278"

data <- get_resource(res_id)

filters <- list("HB" = "S08000030", "Month" = "202109")
wanted_cols <- c("HB", "Month", "TotalPatientsSeen")

filtered_data <- get_resource(
  res_id = res_id,
  row_filters = filters,
  col_select = wanted_cols
)
```
