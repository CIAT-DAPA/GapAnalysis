## R CMD check results

0 errors | 0 warnings | 0 notes

*This is a minor release for version 2.1.0.*

This update introduces a minor method addition and infrastructure improvements:
- Added the `limitByPoints` parameter to `ERSin` and `ERSex` to optionally filter ecoregions and prevent spatial edge effects.
- Clarified throughout the documentation that input coordinates are assumed to be in the WGS84 (EPSG:4326) coordinate reference system.
- Fixed a recurrent `[vect] guessed crs` warning when generating spatial buffers.
- Introduced a comprehensive `testthat` suite to ensure ongoing backwards compatibility for the gap analysis workflows.
- Verified that the package passes `R CMD check --as-cran` with no errors, warnings, or notes.

Thank you for your time and help in reviewing this submission.
