## 2025-05-14 - Batch Processing API Records

**Learning:** Iteratively creating small tibbles from API records in a loop (e.g., `purrr::map` + `tibble::as_tibble`) is extremely slow due to object construction overhead. Furthermore, replacing `NULL` with `""` in individual records can lead to `vctrs` type mismatch errors when columns contain both character and numeric values across different rows.

**Action:** Pre-process the list of records to identify columns containing `NULL`, coerce only those columns to character across all records, and then use a single `dplyr::bind_rows()` call. This avoids redundant object creation and ensures type consistency for high-performance binding.
