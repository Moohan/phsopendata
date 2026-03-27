## 2025-05-15 - Vectorized Context Addition in get_dataset

**Learning:** Iteratively adding context (resource ID, Name, Dates) to individual resource data frames before merging with `list_rbind` is a major bottleneck (~2s for 400 resources). Vectorizing this by merging first and then mapping metadata using a resource index achieves a ~35x speedup.

**Action:** Prefer combining list of data frames first with an index column (`names_to`), then join or map metadata in a single vectorized pass.

**Learning:** Parsing identical date strings for every row in a combined data frame (e.g., thousands of rows) is extremely expensive. Pre-parsing unique date strings once before expansion saves significant time.

**Action:** Always parse unique character vectors to `POSIXct` before using `rep()` or mapping them to a large data frame.

**Learning:** Identifying column type inconsistencies across a list of data frames can be done ~2x faster by unlisting all types and names, then using `split()` and `unique()`, rather than a pairwise loop.

**Action:** Use `split(unlist(types), unlist(names))` for bulk type-consistency checks across large lists of objects.
