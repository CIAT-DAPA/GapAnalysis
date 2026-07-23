# GapAnalysis 2.1.0

* Minor method addition: Added the `limitByPoints` parameter to `ERSin` and `ERSex`. This allows filtering of ecoregions considered in the metrics to only those containing at least one observation. This change was implemented to prevent edge effects where pixels from the distribution extend into neighboring ecoregions as a product of differences in raster/vector geometry rather than being predicted there directly.

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





