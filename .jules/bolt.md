## 2025-05-14 - Robust type resolution for multi-class objects
**Learning:** Using `purrr::map_chr(df, class)` fails when any column has more than one class (e.g., `POSIXct` which has classes `POSIXct` and `POSIXt`).
**Action:** Always use `vapply(df, function(x) class(x)[1], character(1))` to safely extract the primary class of each column in a data frame.
