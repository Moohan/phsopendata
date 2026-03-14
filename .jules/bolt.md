## 2025-05-14 - Vectorized Context Addition
**Learning:** Adding resource context (metadata) iteratively to a list of data frames using `mutate` is extremely slow compared to binding the data frames first and then adding context columns via vectorized repetition.
**Action:** Always prefer adding metadata after `list_rbind` by repeating scalar or resource-level values to match the final row count.

## 2025-05-14 - Global Type Consistency Check
**Learning:** Pairwise column type checking across multiple resources scales poorly. Flattening all column names and types into vectors and using `split()` provides a much faster way to identify type inconsistencies.
**Action:** Use `unlist` and `split` for multi-data-frame schema validation.

## 2025-05-14 - Pre-parsing Dates
**Learning:** Repeatedly parsing the same date strings into `POSIXct` after they have been expanded to millions of rows creates a massive bottleneck.
**Action:** Parse unique date strings *before* expanding them to match the data frame rows.
