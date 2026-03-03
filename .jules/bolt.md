## 2025-01-24 - Efficient type inconsistency checking across many data frames
**Learning:** Checking for column type inconsistencies by iteratively comparing pairs of data frames is O(N) but has high overhead due to repeated `names()` and class extraction. A more efficient O(N) approach is to flatten all names and classes into two vectors using `unlist(..., use.names = FALSE)` and then `split()` by names.
**Action:** Use the vectorized flattening and splitting pattern when comparing schemas across a large list of data frames to avoid loop overhead.

## 2025-01-24 - Vectorized context addition
**Learning:** Adding context (metadata) columns to each data frame in a list before combining is significantly slower than combining first and adding vectorized metadata. Redundant date parsing for the same metadata values also adds overhead.
**Action:** Combine data frames first, expand metadata vectors based on row counts, and perform vectorized column addition once. Pre-parse dates before adding them to the data frame.
