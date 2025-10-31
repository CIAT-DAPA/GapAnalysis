#' @title Geographical representativeness score ex situ
#' @name GRSex
#' @description The GRSex process provides a geographic measurement of the proportion of a species’ range
#'  that can be considered to be conserved in ex situ repositories. The GRSex uses buffers (default 50 km radius)
#'  created around each G coordinate point to estimate geographic areas already well collected within the distribution
#'  models of each taxon, and then calculates the proportion of the distribution model covered by these buffers.
#'
#' @param taxon A character object that defines the name of the species as listed in the occurrence dataset
#' @param sdm a terra rast object that represented the expected distribution of the species
#' @param gBuffer A terra vect which encompases a specific buffer distance around all G points
#'
#' @return A list object containing
#' 1. results : a data frames of values summarizing the results of the function
#' 2. gGaps : a terra vect object showing buffered area about g points
#' 3. map : a leaflet object showing the spatial results of the function
#'
#'

#' @examples
#' ##Obtaining occurrences from example
#' data(CucurbitaData)
#' ##Obtaining Raster_list
#' data(CucurbitaRasts)
#' ##Obtaining protected areas raster
#' data(ProtectedAreas)
#' ## ecoregion features
#' data(ecoregions)
#'
#' # convert the dataset for function
#' taxon <- "Cucurbita_cordata"
#' sdm <- terra::unwrap(CucurbitaRasts)$cordata
#' occurrenceData <- CucurbitaData
#'
#' # generate the g buffer object
#' gBuffer <- generateGBuffers(taxon = taxon,
#'                             occurrenceData = occurrenceData,
#'                             bufferDistM = 50000)
#'
#' #Running GRSex
#' grs_exsitu <- GRSex(taxon = taxon,
#'                     sdm = sdm,
#'                     gBuffer = gBuffer
#'                     )
#'
#'
#'
#' @references
#' Khoury et al. (2019) Ecological Indicators 98:420-429. doi: 10.1016/j.ecolind.2018.11.016
#' Carver et al. (2021) GapAnalysis: an R package to calculate conservation indicators using spatial information
#' @importFrom terra cellSize rasterize mask
#' @importFrom leaflet leaflet addTiles addPolygons addLegend addRasterImage addCircleMarkers
#' @importFrom dplyr tibble
#' @export

GRSex <- function(taxon, sdm, gBuffer) {
  ## all the areas of the cells
  r1 <- terra::cellSize(sdm, unit = "km")
  ## mutliple by origin. values of 1 will retain area measures
  r2 <- r1 * sdm
  totalArea <- sum(values(r2), na.rm = TRUE)

  # clause to see if any g points exist
  if (is.character(gBuffer$data)) {
    grs <- 0
    gArea <- 0
    gMap <- sdm
    map <- leaflet::leaflet()
  } else {
    ## rasterize the object
    b1 <- terra::rasterize(x = gBuffer$data, y = sdm)
    c2 <- r1 * b1 * sdm
    gArea <- sum(values(c2), na.rm = TRUE)

    # gap map
    ## reclass from NA to 0
    b2 <- b1 * -1
    gMap <- terra::mask(x = sdm, b1, inverse = TRUE)

    # clause to determine if any of the buffered area falls within predicted area
    if (gArea == 0) {
      grs <- 0
      gArea <- 0
    } else {
      #calculate GRS
      grs <- min(c(100, gArea / totalArea * 100))
    }
    # leaflet map of results
    map_title <- "<h3 style='text-align:center; background-color:rgba(255,255,255,0.7); padding:2px;'>SDM areas outside of the G Buffer zone</h3>"
    map <- leaflet::leaflet() |>
      leaflet::addTiles() |>
      leaflet::addRasterImage(gMap, colors = "#47b322") |>
      leaflet::addLegend(
        position = "topright",
        title = "GRS ex situ",
        colors = c("#47ae24", "#746fae"),
        labels = c("Distribution", "Buffer G Occurrences"),
        opacity = 1
      ) |>
      leaflet::addControl(html = map_title, position = "bottomleft")
    if (grs > 0) {
      map <- map |>
        leaflet::addRasterImage(
          x = b1,
          colors = "#746fae"
        )
    }
  }

  # create data.frame with output
  out_df <- dplyr::tibble(
    Taxon = taxon,
    'Area of model km2' = totalArea,
    'G buffer areas in model km2' = gArea,
    "GRS exsitu" = grs
  )
  # # leaflet map of results
  # map_title <- "<h3 style='text-align:center; background-color:rgba(255,255,255,0.7); padding:2px;'>SDM areas outside of the G Buffer zone</h3>"
  # map <- leaflet::leaflet() |>
  #   leaflet::addTiles() |>
  #   leaflet::addRasterImage(gMap, colors = "#47b322") |>
  #   leaflet::addLegend(
  #     position = "topright",
  #     title = "GRS ex situ",
  #     colors = c("#47ae24", "#746fae"),
  #     labels = c("Distribution", "Buffer G Occurrences"),
  #     opacity = 1
  #   ) |>
  #   leaflet::addControl(html = map_title, position = "bottomleft")
  # if (grs > 0) {
  #   map <- map |>
  #     leaflet::addRasterImage(
  #       x = b1,
  #       colors = "#746fae"
  #     )
  # }

  #
  output <- list(
    results = out_df,
    gGaps = gMap,
    map = map
  )
  return(output)
}
