## 2025-01-24 - Vectorized Context and Batch Coercion
**Learning:** Vectorizing resource context addition after combining data frames (via list_rbind) and using base R batch coercion instead of dplyr::mutate(across) yields significant performance gains (50x+) and reduces memory overhead in datasets with many resources.
**Action:** Always look for opportunities to combine data first and apply metadata in bulk when dealing with lists of data frames.
