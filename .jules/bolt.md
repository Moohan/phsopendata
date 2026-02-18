# Bolt's Journal - Critical Learnings

## 2025-05-14 - [Vectorized Context Addition]
**Learning:** Vectorizing resource context addition after combining data frames (via `list_rbind`) yielded a 56x speedup (approx. 4ms vs 236ms for 50 resources) and reduced memory allocation by over 75% compared to adding context iteratively. Pre-binding data and then using vectorized indexing is far superior to row-wise or data-frame-wise mutation in loops.
**Action:** Always prefer binding data first with an index column (`names_to`) and then applying metadata in a vectorized manner for multi-resource downloads.

## 2025-05-14 - [Base R Batch Coercion]
**Learning:** Base R batch coercion via `lapply` achieves a ~30x speedup over `dplyr::mutate(across(...))` for lists of many data frames. The overhead of `dplyr`'s NSE and tidy-selection is significant when applied repeatedly in a loop.
**Action:** Use base R subsetting and `lapply` for mass type coercion across lists of data frames in hot paths.
