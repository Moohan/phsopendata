# Lists all available datasets

`list_datasets()` shows all of the datasets hosted on the phs open data
platform.

## Usage

``` r
list_datasets()
```

## Value

A tibble.

## Examples

``` r
head(list_datasets())
#> # A tibble: 6 × 1
#>   name                                                     
#>   <chr>                                                    
#> 1 18-weeks-referral-to-treatment                           
#> 2 27-30-month-review-statistics                            
#> 3 alcohol-related-hospital-statistics-scotland             
#> 4 allied-health-professionals-musculoskeletal-waiting-times
#> 5 allied-health-professional-vacancies                     
#> 6 annual-cancer-incidence                                  
```
