#' @title Geographical representativeness score in situ
#' @name GRSin
#' @description The GRSin process provides a geographic measurement of the proportion of a species’ range that can be considered
#' to be conserved in protected areas. The GRSin compares the area of the distribution model located within protected areas versus
#' the total area of the model, considering comprehensive conservation to have been accomplished only when the entire distribution
#' occurs within protected areas.
#'
#' @param taxon A character object that defines the name of the species as listed in the occurrence dataset
#' @param sdm a terra rast object that represented the expected distribution of the species
#' @param protectedAreas A terra rast object the contian spatial location of protected areas.
#'
#' @return A list object containing
#' 1. results : a data frames of values summarizing the results of the function
#' 2. protectAreaMask : a terra rast object showing all the protected areas within the distribution
#' 3. map : a leaflet object showing the spatial results of the function
#'
#' @examples
#' ##Obtaining Raster_list
#' data(CucurbitaRasts)
#' ##Obtaining protected areas raster
#' data(ProtectedAreas)
#'
#' # convert the dataset for function
#' taxon <- "Cucurbita_cordata"
#' sdm <- terra::unwrap(CucurbitaRasts)$cordata
#' protectedAreas <- terra::unwrap(ProtectedAreas)
#'
#' #Running GRSin
#' grs_insitu <- GRSin(taxon = taxon,
#'                     sdm = sdm,
#'                     protectedAreas = protectedAreas
#'                     )
#'
#'
#'
#' @references
#' Khoury et al. (2019) Ecological Indicators 98:420-429. doi: 10.1016/j.ecolind.2018.11.016
#' Carver et al. (2021) GapAnalysis: an R package to calculate conservation indicators using spatial information
#' @importFrom terra crop expanse
#' @importFrom dplyr tibble
#' @importFrom leaflet addTiles addPolygons addLegend addRasterImage addCircleMarkers
#' @importFrom magrittr %>%
#' @export

GRSin <- function(taxon, sdm, protectedAreas){
  # total area of the SDM inside protected areas over
  # total aras of the SDM

  # crop protected areas to sdm
  pro <- terra::crop(protectedAreas, sdm)
  # mask to model
  proMask <- pro * sdm

  # generate areas
  sdmArea <- terra::expanse(sdm,unit = "km")[,2]
  proArea <- terra::expanse(proMask,unit = "km")[,2]

  # calcualte the total area
  if(proArea == 0){
    grs <- 0
  }else{
    grs <- min(c(100, proArea/sdmArea*100))
  }
  # return objects
  df_output <- dplyr::tibble(Taxon = taxon,
                   'Area of model km2' = round(sdmArea, digits = 0),
                   'Area in protected ares km2' = round(proArea, digits = 0),
                   "GRS insitu" = grs)

  map_title <- "<h3 style='text-align:center; background-color:rgba(255,255,255,0.7); padding:2px;'>Protect areas within the SDM</h3>"
  map <- leaflet::leaflet() |>
    leaflet::addTiles() |>
    leaflet::addRasterImage(
      x = sdm,
      colors = "#47ae24"
    )|>
    leaflet::addRasterImage(
      x = proMask,
      colors = "#746fae"
    )|>
    leaflet::addLegend(
      position = "topright",
      title = "GRS in situ",
      colors = c("#47ae24","#746fae"),
      labels = c("Distribution","Protected Areas"),
      opacity = 1
    )|>
    leaflet::addControl(html = map_title, position = "bottomleft")

  # create output data
  output <- list(
    results = df_output ,
    protectAreaMask = proMask,
    map = map
  )
  return(output)
}
