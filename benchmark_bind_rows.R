
library(dplyr)
library(purrr)
library(bench)

records <- list(
  list(a = 1, b = "x"),
  list(a = 2, b = "y"),
  list(a = 3, b = "z")
)

# Benchmark redundant map
bm <- bench::mark(
  with_map = purrr::map(records, ~.x) %>% dplyr::bind_rows(),
  without_map = dplyr::bind_rows(records),
  iterations = 1000
)

print(bm)
