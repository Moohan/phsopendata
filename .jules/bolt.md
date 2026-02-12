# Bolt's Performance Journal

## 2025-05-15 - [Vectorized Context Addition] **Learning:** Iterative metadata addition using `dplyr::mutate` and `pmap` scales poorly with the number of resources in `get_dataset`. Vectorizing this by combining data first and then applying metadata once yields ~45x speedup. **Action:** Always combine resource data before adding common metadata columns.

## 2025-05-15 - [Efficient Type Resolution] **Learning:** Using `purrr::map` and loops to find type inconsistencies across a list of data frames is significantly slower than the base R pattern of `do.call(c, ...)` followed by `split()` and `vapply()`. **Action:** Prefer base R grouping/splitting for multi-dataframe type analysis.
