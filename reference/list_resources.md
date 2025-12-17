# Lists all available resources for a dataset

`list_resources()` returns all of the resources associated with a
dataset

## Usage

``` r
list_resources(dataset_name)
```

## Arguments

- dataset_name:

  name of the dataset as found on [NHS Open Data
  platform](https://www.opendata.nhs.scot/)

## Value

a [tibble](https://tibble.tidyverse.org/reference/tibble-package.html)
with the data

## Examples

``` r
list_resources("weekly-accident-and-emergency-activity-and-waiting-times")
#> # A tibble: 1 × 4
#>   res_id                           name  created             last_modified      
#>   <chr>                            <chr> <dttm>              <dttm>             
#> 1 a5f7ca94-c810-41b5-a7c9-25c18d4… Week… 2023-05-02 07:57:54 2025-12-16 10:03:24
```
