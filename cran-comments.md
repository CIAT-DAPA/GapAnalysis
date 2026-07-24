## R CMD check results

0 errors | 0 warnings | 0 notes

*This is a patch release for version 2.1.1.*

This update introduces several fixes and documentation improvements:
- Integrated spelling, grammar, and style standardizations throughout the documentation and README.
- Introduced robust handling of the "no-model" case inside `ERSex` and `ERSin` to safely return valid outputs when a species distribution model is missing.
- Added ecoregion polygon aggregation by ID to optimize spatial intersection logic.
- Resolved R CMD check warnings regarding undocumented arguments by updating Roxygen tags and regenerating package documentation.
- Declared package global variables to ensure clean checks on CRAN.

Thank you for your time and help in reviewing this submission.
