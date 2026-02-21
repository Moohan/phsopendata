## 2025-05-15 - [Optimization Suite for get_dataset and get_resource]

**Learning:** Vectorizing resource context addition after combining data frames (via list_rbind) yielded a 56x speedup (approx. 4ms vs 236ms for 50 resources) and reduced memory allocation by over 75% compared to adding context iteratively with dplyr::mutate. Also, global type resolution using split() is more robust and faster than pairwise consecutive checks.

**Action:** Always prefer vectorized operations on the combined data frame over iterative modifications to elements of a list of data frames. Use base R for hot-path column assignments and coercion.
