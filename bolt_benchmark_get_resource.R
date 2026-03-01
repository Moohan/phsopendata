
library(dplyr)
library(purrr)
library(bench)

# Simulate records from CKAN API
n <- 1000
records <- lapply(1:n, function(i) {
  list(
    col1 = i,
    col2 = paste0("val", i),
    col3 = Sys.Date()
  )
})

bm <- bench::mark(
  current = {
    purrr::map(records, ~.x) %>%
      dplyr::bind_rows()
  },
  optimized = {
    dplyr::bind_rows(records)
  },
  iterations = 100
)

print(bm)
