## R CMD check results

0 errors | 0 warnings | 1 note

*This is a version update of an existing library 

Thank you for your efforts :) 

The previous version of this package utilized the `sp` and `raster` libraries. 

This update is primary to transition all spatial data workflows to the currently supported libraries of `terra` and `sf`

As it's a major update we've also taken the time to add some quality of life projects to the primary functions to give end users more evaluatory outputs, i.e. maps. 


NOTE 
- the note defined from the devtools::check(cran=TRUE) is related to no visible binding for global variable. This appears in multiple functions and is related to the use of column/object names within the piping structures, generally connected to the dplyr functions. 

