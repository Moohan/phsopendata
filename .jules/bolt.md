## 2025-01-24 - Vectorized Type Checking in get_dataset
**Learning:** The original pairwise loop for type consistency check was $O(N)$ with high overhead and failed on multi-class objects like POSIXct. Vectorizing with `unlist` and `split` is ~4x faster and more robust.
**Action:** Use `vapply(df, function(x) class(x)[1L], character(1L))` and `split()` to identify type inconsistencies across lists of data frames.
