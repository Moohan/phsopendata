# phsopendata (development version)

- Removed 'magrittr' dependency and switched to base R pipe '|>'. Minimum R version increased to 4.1.0.

# phsopendata 1.1.0 (2026-06-05)

- [`list_resources()`](https://public-health-scotland.github.io/phsopendata/reference/list_resources.html) has been upgraded. Instead of just returning all resources from a dataset (given the exact title), it can now search both resources and dataset titles and will list all resources that match the search!
- Csilla is back, so I've reinstated her as the package maintainer.

# phsopendata 1.0.3 (2026-02-05)

- No user-facing changes. Strengthened examples and tests, particularly when ran on CRAN.

# phsopendata 1.0.2 (2026-01-29)

- No user-facing changes. Update the maintainer (temporarily) to James Hayes (james.hayes2@phs.scot) while Csilla is on a career break!

# phsopendata 1.0.1 (2025-11-10)

- No user-facing changes. Fixes some tests that were failing when the open data platform is offline, and fixes tests that were failing due to a change in the open data resource. Tests are now more robust against changes to the data.  

# phsopendata 1.0.0 (2025-09-03)

- Initial release.
