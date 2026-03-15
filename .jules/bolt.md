## 2025-05-15 - [Vectorized Context Addition and Robust Type Checking]

**Learning:** Iteratively adding context columns (Resource ID, Name, etc.) to individual data frames before combining them via `list_rbind` is significantly slower and more memory-intensive than combining first and vectorizing the context addition. Additionally, using `purrr::map_chr(df, class)` is fragile because certain R classes (like `POSIXct`) return a character vector of length > 1, causing `map_chr` to fail.

**Action:** Always combine data frames before applying row-level metadata if possible. Use `vapply(df, function(x) class(x)[1L], character(1L))` to robustly identify primary column types for consistency checks. Use base R `lapply` for batch type coercion to avoid the overhead of `dplyr::mutate` in hot loops.

## 2025-05-16 - [Robustness in API Data Extraction]

**Learning:** Using `1:n` for indexing is dangerous when `n` can be 0, as it creates a descending sequence `1, 0`. Additionally, when using `try()` around `phs_GET`, the result can be a `try-error` atomic vector; attempting to access list elements like `$result` on it will crash the process with "operator is invalid for atomic vectors".

**Action:** Always use `seq_len()` for safe indexing. Explicitly check for `inherits(content, 'try-error')` before attempting to access list components of an API response.
