# Bolt Journal ⚡

## 2026-02-13 - [Vectorized Context Addition]
**Learning:** iterative context addition using `purrr::pmap` over a list of data frames is extremely slow (O(N*M) where N is resources and M is rows). Binding all data frames first with `purrr::list_rbind(..., names_to = "res_idx")` and then applying context in a single vectorized operation using `res_idx` to index metadata is ~30x faster and significantly more memory efficient.
**Action:** Always prefer binding data before adding resource-specific metadata in `get_dataset` style functions.

## 2026-02-13 - [Robust Type Resolution]
**Learning:** `purrr::map_chr(df, class)` crashes on columns with multiple classes (e.g., `POSIXct`). Using `vapply(df, function(col) class(col)[1L], character(1L))` is robust and faster.
**Action:** Use `vapply` with `class()[1L]` for column type identification.
