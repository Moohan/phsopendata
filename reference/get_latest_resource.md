# Get the latest resource from a data set

Returns the latest resource available in a dataset.

## Usage

``` r
get_latest_resource(
  dataset_name,
  rows = NULL,
  row_filters = NULL,
  col_select = NULL,
  include_context = TRUE
)
```

## Arguments

- dataset_name:

  name of the dataset as found on [NHS Open Data
  platform](https://www.opendata.nhs.scot/)

- rows:

  (optional) specify the max number of rows to return for each resource.

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

## Details

There are some datasets on the open data platform that keep historic
resources instead of updating existing ones. For these it is useful to
be able to retrieve the latest resource. As of 1.8.2024 these data sets
include:

- gp-practice-populations

- gp-practice-contact-details-and-list-sizes

- nhsscotland-payments-to-general-practice

- dental-practices-and-patient-registrations

- general-practitioner-contact-details

- prescribed-dispensed

- dispenser-location-contact-details

- community-pharmacy-contractor-activity

## Examples

``` r
dataset_name <- "gp-practice-contact-details-and-list-sizes"

data <- get_latest_resource(dataset_name)

filters <- list("Postcode" = "DD11 1ES")
wanted_cols <- c("PracticeCode", "Postcode", "Dispensing")

filtered_data <- get_latest_resource(
  dataset_name = dataset_name,
  row_filters = filters,
  col_select = wanted_cols
)
```
