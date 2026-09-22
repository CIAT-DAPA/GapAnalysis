# GapAnalysis 2.1.1

* Method addition: The workflow now supports taxa for which no species distribution model is available (for example, taxa with records but no usable coordinates). `SRSin`, `ERSin`, and `ERSex` accept `sdm = NULL`; `SRSin` then evaluates all occurrence points against protected areas, and the ERS metrics return 0 with the ecoregion counts set accordingly. The results tables and leaflet maps reflect the missing model.
* Method addition: Added the `noModel` parameter to `FCSex` and `FCSin`. When `TRUE`, `GRS` and `ERS` are assigned 0 and the final conservation score is the mean of the three metrics, consistent with the treatment of unmodelled taxa in Khoury et al. (2019) <doi:10.1016/j.ecolind.2018.11.016>. `GRSex` and `GRSin` require a model and should be skipped for these taxa.
* Bug fix: `getDatasets` wrote the ecoregions file into the protected areas folder and used a misspelled filename for the protected areas raster. Both paths are corrected, and the function gains an `out_dir` argument (defaulting to the previous `tools::R_user_dir()` location) and returns the file paths invisibly.
* Bug fix: `ERSex` and `ERSin` no longer error when `limitByPoints = TRUE` leaves no ecoregions to evaluate.
* Infrastructure: Replaced data-masked column references in `ERSex` with the `.data` pronoun.
* Infrastructure: Added a `testthat` file covering taxa with no coordinates, taxa with no model, taxa with no G records, and the `noModel` behaviour of `FCSex` and `FCSin`.

# GapAnalysis 2.1.0

* Minor method addition: Added the `limitByPoints` parameter to `ERSin` and `ERSex`. This allows filtering of ecoregions considered in the metrics to only those containing at least one observation. This change was implemented to prevent edge effects where pixels from the distribution extend into neighboring ecoregions as a product of differences in raster/vector geometry rather than being predicted there directly.
* Documentation: Clarified throughout the function documentation (e.g., `@param occurrenceData` and `@param csv`) and in the `README.md` that all input coordinates are assumed to be in the WGS84 (EPSG:4326) coordinate reference system.
* Bug fix: Resolved a recurrent `[vect] guessed crs` warning in `generateGBuffers` by explicitly defining the CRS during spatial vector creation.
* Infrastructure: Introduced a comprehensive `testthat` suite to automatically test the full ex-situ and in-situ workflows and ensure ongoing backwards compatibility.

# GapAnalysis 2.0.2

* Fixed several typos in exported data frames (`FCS existu score` to `FCS exsitu score`, `Area in protected ares km2` to `Area in protected areas km2`, and `Total with cooordinates` to `Total with coordinates`).
* Updated vignette and README to reflect these fixes and clarify data storage locations.
* Bumped version for CRAN resubmission.

# GapAnalysis 2.0.1

* Fixed CRAN notes regarding insecure URLs by updating `http:` links to `https:`.
* Fixed CRAN notes regarding DOI formatting by replacing plain text or `\url{doi:...}` formats with the proper `\doi{...}` macro in all documentation.

# GapAnalysis 2.0.0

* Major update and re-submission to CRAN addressing previous archival issues.

Fundamentally the gap analysis functions will produce the same quantitative results as the first version of the package

Specific changes 

- Workflows use `sf` and `terra` libraries 

- Individuals functions produce leaflet maps to visualize outputs 

- removed the `summaryHTML` function for the initial release. This should be added again in later updates. 





