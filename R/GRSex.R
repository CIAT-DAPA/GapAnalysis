
#' @title Geographical representativeness score ex situ
#' @name GRSex
#' @description The GRSex process provides a geographic measurement of the proportion of a species’ range
#'  that can be considered to be conserved in ex situ repositories. The GRSex uses buffers (default 50 km radius)
#'  created around each G coordinate point to estimate geographic areas already well collected within the distribution
#'  models of each taxon, and then calculates the proportion of the distribution model covered by these buffers.
#' @param taxon
#'
#' @param sdm
#' @param gBuffer
#'
#'
#' @return
#'
#' @examples
#'
#'
#' @references
#' Khoury et al. (2019) Ecological Indicators 98:420-429. doi: 10.1016/j.ecolind.2018.11.016
#' Carver et al. (2021) GapAnalysis: an R package to calculate conservation indicators using spatial information
GRSex <- function(taxon, sdm, gBuffer) {
  ## all the areas of the cells
  r1 <- cellSize(sdm,unit="km")
  ## mutliple by origin. values of 1 will retain area measures
  r2 <- r1 * sdm
  totalArea <- sum(values(r2), na.rm = TRUE)

  # clause to see if any g points exist
  if(class(gBuffer) == "character"){ # not sure if this is the best condition
    grs <- 0
    gArea <- 0
  }else{
    ## rasterize the object
    b1 <- terra::rasterize(x = gBuffer, y = sdm)
    c2 <- r1 * b1 * sdm
    gArea <- sum(values(c2), na.rm = TRUE)

    # gap map
    ## reclass from NA to 0
    b2 <- b1 * -1
    gMap <- terra::mask(x = sdm, b1, inverse=TRUE )

    # clause to determine if any of the buffered area falls within predicted area
    if(gArea == 0){
      grs <- 0
      gArea <- 0
    }else{
      #calculate GRS
      grs <- min(c(100, gArea/totalArea*100))
    }
  }

  # create data.frame with output
  out_df <- dplyr::tibble(Taxon=taxon,
                       'Area of model km2'=totalArea,
                       'G buffer areas in model km2' =gArea,
                       "GRS exsitu" =grs)
  # leaflet map of
  map_title <- "<h3 style='text-align:center; background-color:rgba(255,255,255,0.7); padding:2px;'>SDM areas outside of the G Buffer zone</h3>"
  map <- leaflet() |>
    addTiles() |>
    addRasterImage(gMap,
                   colors = "#47b322")|>
    addLegend(
      position = "topright",
      title = "GRS ex situ",
      colors = c("#47ae24","#746fae"),
      labels = c("Distribution","Buffer G Occurrences"),
      opacity = 1
    )|>
    addControl(html = map_title, position = "bottomleft")
  if(grs >0 ){
    map <- map |>
      addRasterImage(
        x = b1,
        colors = "#746fae"
      )
  }



  #
  output <- list(
    results = out_df,
    gGaps = gMap,
    map = map
  )
  return(output)
}

