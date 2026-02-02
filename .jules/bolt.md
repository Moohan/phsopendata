## 2025-05-15 - Vectorized Context and Fast Coercion in get_dataset
**Learning:** iterative dplyr::mutate calls and type checks using tibble/dplyr pipelines are significantly slower than vectorized base R patterns and join-based metadata addition for large lists of data frames.
**Action:** Use lapply + base R coercion and bind-first-then-join patterns for processing multiple resources.
