## 2025-05-14 - Vectorized Context Addition and Efficient Type Resolution
**Learning:** In `get_dataset`, adding resource metadata in a loop for each data frame is much slower than binding all data frames first and then prepending metadata in a vectorized way using `rep()` and `bind_cols()`. Type resolution across many data frames is also significantly faster when using a base R `split()` and `vapply()` pattern rather than a tibble-based `group_by()` and `summarise()` approach.
**Action:** Always prefer post-binding vectorized metadata application for multi-resource extraction. Use base R grouping patterns for high-performance type checking across lists of data frames.

## 2025-05-14 - Robustness in Vectorized Date Operations
**Learning:** When vectorizing date comparisons (e.g., ensuring `CreatedDate <= ModifiedDate`), the logical comparison `m < c` returns `NA` if either date is `NA`. Using this result for indexing errors in R.
**Action:** Use `!is.na(m) & !is.na(c) & m < c` and explicitly set `NA` results to `FALSE` (e.g., `idx[is.na(idx)] <- FALSE`) before using it for assignment to ensure robustness.
