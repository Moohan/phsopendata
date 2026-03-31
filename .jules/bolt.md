# BOLT'S JOURNAL - CRITICAL LEARNINGS ONLY

## 2024-05-23 - [Optimization] Iterative `as_tibble` in SQL record processing
**Learning:** Calling `tibble::as_tibble()` in a loop (e.g. `purrr::map`) for every record returned by a database or API query is a significant performance bottleneck. For a result set of 10,000 records, the iterative approach is ~7x slower and allocates significantly more memory than `dplyr::bind_rows()` on the raw list of records.
**Action:** Always prefer `dplyr::bind_rows()` on a list of record lists, followed by vectorized column-wise post-processing for NULL/NA handling.
