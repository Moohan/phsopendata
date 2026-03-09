## 2025-01-24 - Vectorized Context Addition in get_dataset

**Learning:** Adding resource metadata (context) to each individual resource data frame before combining them with `purrr::list_rbind()` is a major bottleneck. Repeatedly calling `as.POSIXct` on the same date strings and performing many small `dplyr::mutate()` operations on individual data frames is extremely inefficient. Vectorizing this by parsing dates once and using `rep()` to expand metadata after a single `list_rbind()` call achieved a ~56x speedup.

**Action:** Always prefer combining data first and adding metadata in a vectorized way when processing lists of data frames.
