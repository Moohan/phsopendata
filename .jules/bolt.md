# Bolt's Journal - Critical Learnings

## 2025-05-14 - Vectorized Context Addition and Type Resolution

**Learning:** Iteratively calling `dplyr::mutate()` and `purrr::pmap()` to add resource context to many data frames is a major bottleneck (O(N) data copies). Vectorizing this after `list_rbind()` by subsetting metadata vectors with indices is ~25x-45x faster. Similarly, type resolution using a "flatten and split" pattern is significantly faster than adjacent-pair loops.

**Action:** Always prefer vectorizing metadata addition and type checks after combining lists of data frames. Use `names_to` in `list_rbind` to facilitate this mapping.

## 2025-05-14 - Robust Date Parsing

**Learning:** Using locale-dependent formats like `%X` in `as.POSIXct` can lead to inconsistencies and precision loss. Strict ISO8601 parsing with `%Y-%m-%dT%H:%M:%OS` is more robust and correctly handles sub-second precision which is necessary for consistency between `get_resource` and `get_dataset`.

**Action:** Use `%Y-%m-%dT%H:%M:%OS` when parsing CKAN resource dates.
