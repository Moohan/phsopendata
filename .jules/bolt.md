# Bolt's Journal - Critical Learnings

## 2025-01-24 - [Vectorized Context Addition]
**Learning:** Moving resource context addition (ResID, ResName, etc.) from an iterative `pmap` call on a list of data frames to a vectorized operation after combining them with `list_rbind(..., names_to = 'res_idx')` yields a ~50x speedup for datasets with many resources.
**Action:** Always prefer combining data frames first and adding metadata in a vectorized fashion using indexing.

## 2025-01-24 - [Base R Coercion for Large Lists]
**Learning:** Using base R `lapply` with `df[cols] <- lapply(df[cols], as.character)` is significantly faster (~30x) than using `dplyr::mutate(across(...))` when iterating over a large list of data frames, as it avoids the overhead of dplyr's NSE and internal machinery.
**Action:** Use base R for simple batch operations inside loops or `lapply` calls over many objects.

## 2025-01-24 - [Robust Type Resolution]
**Learning:** Comparing column classes only between adjacent data frames in a list can miss inconsistencies (e.g., if a column is missing in intermediate data frames). A global check across all resources is more robust.
**Action:** Use a global name-class map to identify type inconsistencies across all resources in `get_dataset`.
