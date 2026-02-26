## 2026-02-26 - Vectorized Context Addition Speedup

**Learning:** Vectorizing resource context addition after combining data frames (via `list_rbind`) yielded a 56x speedup (approx. 4ms vs 236ms for 50 resources) and reduced memory allocation by over 75% compared to adding context iteratively using `purrr::pmap`.

**Action:** Always prefer combining data first and applying metadata/context in a single vectorized step when dealing with multiple resources.

## 2026-02-26 - Pre-parsing Dates for Vectorized Operations

**Learning:** To avoid massive performance penalties in vectorized context addition, pre-parse date character vectors into `POSIXct` objects *before* expanding them to match the combined data frame rows. Repeatedly parsing the same date strings after they have been recycled to match a large data frame creates significant overhead.

**Action:** Pre-calculate and parse invariants (like metadata dates) before row expansion or vectorized assignment.

## 2026-02-26 - Direct bind_rows Performance

**Learning:** Directly passing a list of records from an API response to `dplyr::bind_rows()` in `get_resource.R` is ~50% more memory-efficient and faster than mapping an identity function (`purrr::map(records, ~.x)`) over the list first.

**Action:** Avoid redundant `purrr::map` calls when the target function (like `bind_rows`) already handles lists efficiently.
