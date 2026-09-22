## R CMD check results

0 errors | 0 warnings | 0 notes

*This is a patch release, version 2.1.1.*

This update extends the workflow to taxa for which no species distribution model is available and fixes two bugs:

- `SRSin`, `ERSin`, and `ERSex` accept a missing model (`sdm = NULL`) and return results consistent with the published method.
- Added the `noModel` argument to `FCSex` and `FCSin`. When no model exists, `GRS` and `ERS` are assigned 0 and the final score is the mean of the three metrics, following Khoury et al. (2019).
- Fixed `getDatasets`, which wrote the ecoregions file to the wrong folder and used a misspelled raster filename. The function gains an optional `out_dir` argument; the default location is unchanged.
- Fixed an error in `ERSex` and `ERSin` when `limitByPoints = TRUE` left no ecoregions.
- Added a `testthat` file exercising the no-model, no-coordinate, and no-G-record conditions.

No changes were made to existing function signatures other than the new optional arguments with defaults.

Thank you for your time and help in reviewing this submission.
