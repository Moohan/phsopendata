# Get Open Data resources from a dataset

Get Open Data resources from a dataset

## Usage

``` r
get_dataset(
  dataset_name,
  max_resources = NULL,
  rows = NULL,
  row_filters = NULL,
  col_select = NULL,
  include_context = FALSE
)
```

## Arguments

- dataset_name:

  name of the dataset as found on [NHS Open Data
  platform](https://www.opendata.nhs.scot/)

- max_resources:

  (optional) the maximum number of resources to return, use for testing
  code, it will return the n latest resources

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

## See also

[`get_resource()`](https://public-health-scotland.github.io/phsopendata/reference/get_resource.md)
for downloading a single resource from a dataset.

## Examples

``` r
get_dataset("gp-practice-populations",
  max_resources = 2, rows = 10
)
#> # A tibble: 20 × 59
#>        Date PracticeCode HB       HSCP  Sex   SexQF AllAges AllAgesQF Ages00to04
#>       <int>        <int> <chr>    <chr> <chr> <chr>   <int> <chr>          <int>
#>  1 20251001        10002 S080000… S370… Fema… ""       4328 ""               175
#>  2 20251001        10002 S080000… S370… Male  ""       4163 ""               174
#>  3 20251001        10002 S080000… S370… All   "d"      8491 ""               349
#>  4 20251001        10017 S080000… S370… Fema… ""       3850 ""               119
#>  5 20251001        10017 S080000… S370… Male  ""       3856 ""               130
#>  6 20251001        10017 S080000… S370… All   "d"      7706 ""               249
#>  7 20251001        10036 S080000… S370… Fema… ""       2393 ""                57
#>  8 20251001        10036 S080000… S370… Male  ""       2468 ""                86
#>  9 20251001        10036 S080000… S370… All   "d"      4861 ""               143
#> 10 20251001        10106 S080000… S370… Fema… ""       3318 ""                86
#> 11 20250701        10002 S080000… S370… Male  ""       4134 ""                NA
#> 12 20250701        10002 S080000… S370… Fema… ""       4292 ""                NA
#> 13 20250701        10002 S080000… S370… All   "d"      8426 ""                NA
#> 14 20250701        10017 S080000… S370… Male  ""       3822 ""                NA
#> 15 20250701        10017 S080000… S370… Fema… ""       3854 ""                NA
#> 16 20250701        10017 S080000… S370… All   "d"      7676 ""                NA
#> 17 20250701        10036 S080000… S370… Male  ""       2456 ""                NA
#> 18 20250701        10036 S080000… S370… Fema… ""       2402 ""                NA
#> 19 20250701        10036 S080000… S370… All   "d"      4858 ""                NA
#> 20 20250701        10106 S080000… S370… Male  ""       3115 ""                NA
#> # ℹ 50 more variables: Ages00to04QF <chr>, Ages05to09 <int>,
#> #   Ages05to09QF <chr>, Ages10to14 <int>, Ages10to14QF <chr>, Ages15to19 <int>,
#> #   Ages15to19QF <chr>, Ages20to24 <int>, Ages20to24QF <chr>, Ages25to29 <int>,
#> #   Ages25to29QF <chr>, Ages30to34 <int>, Ages30to34QF <chr>, Ages35to39 <int>,
#> #   Ages35to39QF <chr>, Ages40to44 <int>, Ages40to44QF <chr>, Ages45to49 <int>,
#> #   Ages45to49QF <chr>, Ages50to54 <int>, Ages50to54QF <chr>, Ages55to59 <int>,
#> #   Ages55to59QF <chr>, Ages60to64 <int>, Ages60to64QF <chr>, …
```
