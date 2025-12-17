# get a datasets additional info

`get_dataset_additional_info()` returns a tibble of dataset names along
with the amount of resources it has and the date it was last
updated.Last updated is taken to mean the most recent date a resource
within the dataset was created or modified.

## Usage

``` r
get_dataset_additional_info(dataset_name)
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
get_dataset_additional_info("gp-practice-populations")
#> # A tibble: 1 × 3
#>   name                    n_resources last_updated       
#>   <chr>                         <int> <dttm>             
#> 1 gp-practice-populations          48 2025-10-22 15:50:32
```
