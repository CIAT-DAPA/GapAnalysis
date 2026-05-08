## R CMD check results

0 errors | 0 warnings | 1 note (HTML Tidy, depending on system configuration)

*This is a patch release for version 2.0.0.*

This update addresses the minor formatting notes raised by the CRAN team during the 2.0.0 submission:
- Converted all DOI references in `.R` and `.Rd` files to use the `\doi{}` macro instead of `\url{doi:}`.
- Converted insecure `http://` URLs in the README to `https://`.
- Replaced a broken dataset link with the current functional one.

Thank you for your efforts :)
