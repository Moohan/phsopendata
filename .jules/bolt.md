# BOLT ⚡ JOURNAL - CRITICAL LEARNINGS ONLY

## 2025-01-30 - Vectorized context addition in `get_dataset.R`

**Learning:** Vectorizing the addition of resource context columns *after* combining data frames (via `list_rbind`) achieved a ~20x performance improvement compared to the iterative `pmap` + `add_context` approach. For 50 resources, the execution time dropped from ~215ms to ~9ms.

**Action:** Always prefer combining lists of data frames before adding shared or repeating metadata columns to maximize the benefits of `dplyr::mutate` vectorization.

## 2025-01-30 - Vectorized type consistency check

**Learning:** Replacing a pairwise loop that compares column types across resource data frames with a vectorized `unlist` + `split` + `vapply(length(unique(...)))` approach reduced the check time by ~75% (e.g., from ~20ms to ~5ms for 400 resources). This scales much better with the number of resources.

**Action:** Use `split()` to group metadata (like types) by key (like column names) for efficient aggregate checks across many objects.

## 2025-01-30 - Efficient resource extraction in `get_resource.R`

**Learning:** Passing the list of records from an API response directly to `dplyr::bind_rows()` is more memory-efficient and slightly faster than mapping an identity function `~.x` over the list first.

**Action:** Avoid redundant `purrr::map(..., ~.x)` calls before binding rows.
