% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_resource_sql.R
\name{get_resource_sql}
\alias{get_resource_sql}
\title{Get PHS Open Data using SQL}
\usage{
get_resource_sql(sql)
}
\arguments{
\item{sql}{(character) a single PostgreSQL SELECT query.

Must include a resource ID, which must be double-quoted
e.g.,

\verb{SELECT * from "58527343-a930-4058-bf9e-3c6e5cb04010"}

Column names must be double-quoted,
while character values in filters must be single-quoted.
e.g.,

\code{"Age" = '34'}

You may need to escape quote marks with \verb{\\} to implement this. e.g.,

\verb{sql = "SELECT * FROM \\"<res_id>\\" WHERE \\"Age\\" = '34'"}.}
}
\value{
a \link[tibble:tibble-package]{tibble} with the query results.
Only 32,000 rows can be returned from a single SQL query.
}
\description{
Similar to \code{\link[=get_resource]{get_resource()}}
but with more options for server-side querying of data.
However, this function has a lower maximum row number
(32,000 vs 99,999) for returned results.
}
\examples{
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
}
\seealso{
\code{\link[=get_resource]{get_resource()}} for downloading a resource without using a
SQL query.
}
