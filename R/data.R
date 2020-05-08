#' @title Cucurbita occurrences dataset
#' @name CucurbitaData
#' @docType data
#' @description This dataset is a subset of the original dataset for:
#'  C. cordata, C. digitata and C. palmata used in Khoury et al., 2020
#' @format A data frame with 1184 rows and 4 variables:
#' \describe{
#'   \item{taxon}{character: Species name}
#'   \item{latitude}{numeric:Latitude in decimal format}
#'   \item{longitude}{numeric: Longitude in decimal format}
#'   \item{type}{character: Source of the record,germplasm (G) or herbarium (H)}
#' }
#' @references
#' Khoury, CK, Carver, D, Kates, HR, et al. Distributions, conservation status,
#' and abiotic stress tolerance potential of wild cucurbita (Cucurbita L.).
#' Plants, People, Planet. 2019; 00: 1– 15. https://doi.org/10.1002/ppp3.10085
#' @source \url{https://dataverse.harvard.edu/dataverse/GapAnalysisR}
"CucurbitaData"

#' @title Cucurbita species distribution models dataset
#' @name CucurbitaRasters
#' @docType data
#' @format raster files
#' @description This dataset is a subset of species distribution models for:
#'  C. cordata, C. digitata and C. palmata used in Khoury et al., 2020
#' @references
#' Khoury, CK, Carver, D, Kates, HR, et al. Distributions, conservation status,
#'  and abiotic stress tolerance potential of wild cucurbita (Cucurbita L.).
#'  Plants, People, Planet. 2019; 00: 1– 15. https://doi.org/10.1002/ppp3.10085
#' @source \url{https://dataverse.harvard.edu/dataverse/GapAnalysisR}
"CucurbitaRasters"

#' @title Ecoregions shapefile
#' @name ecoregions
#' @docType data
#' @format Shapefile
#' @description This dataset is a subset of the Terrestrial Ecoregions shapefile made by the Nature Conservancy
#' @source \url{http://maps.tnc.org/gis_data.html}
"ecoregions"

#' @title Protected areas dataset in raster format
#' @name  ProtectedAreas
#' @docType data
#' @format Raster file
#' @description This dataset is a raster version of the world protected areas dataset used in Khoury et al., (2019)
#' @source \url{https://www.protectedplanet.net/}
#' @references
#' Khoury, C. K., Amariles, D., Soto, J. S., Diaz, M. V., Sotelo, S., Sosa, C. C., … Jarvis, A. (2019).
#' Comprehensiveness of conservation of useful wild plants: An operational indicator for biodiversity
#' and sustainable development targets. Ecological Indicators. https://doi.org/10.1016/j.ecolind.2018.11.016
"ProtectedAreas"
