# Get PHS Open Data using SQL

Similar to
[`get_resource()`](https://public-health-scotland.github.io/phsopendata/reference/get_resource.md)
but with more options for server-side querying of data. However, this
function has a lower maximum row number (32,000 vs 99,999) for returned
results.

## Usage

``` r
get_resource_sql(sql)
```

## Arguments

- sql:

  (character) a single PostgreSQL SELECT query.

  Must include a resource ID, which must be double-quoted e.g.,

  `SELECT * from "58527343-a930-4058-bf9e-3c6e5cb04010"`

  Column names must be double-quoted, while character values in filters
  must be single-quoted. e.g.,

  `"Age" = '34'`

  You may need to escape quote marks with `\` to implement this. e.g.,

  `sql = "SELECT * FROM \"<res_id>\" WHERE \"Age\" = '34'"`.

## Value

a [tibble](https://tibble.tidyverse.org/reference/tibble-package.html)
with the query results. Only 32,000 rows can be returned from a single
SQL query.

## See also

[`get_resource()`](https://public-health-scotland.github.io/phsopendata/reference/get_resource.md)
for downloading a resource without using a SQL query.

## Examples

``` r
sql <- "
   SELECT
     \"TotalCancelled\",\"TotalOperations\",\"Hospital\",\"Month\"
   FROM
     \"bcc860a4-49f4-4232-a76b-f559cf6eb885\"
   WHERE
     \"Hospital\" = 'D102H'
"
df <- get_resource_sql(sql)

# This is equivalent to:
cols <- c(
  "TotalCancelled", "TotalOperations",
  "Hospital", "Month"
)
row_filter <- c(Hospital = "D102H")

df2 <- get_resource(
  "bcc860a4-49f4-4232-a76b-f559cf6eb885",
  col_select = cols,
  row_filters = row_filter
)
```
