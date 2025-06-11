
#' Title
#'
#' @param taxon
#' @param sdm
#' @param protectedAreas
#'
#' @return
#' @export
#'
#' @examples
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
  map <- leaflet() |>
    addTiles() |>
    addRasterImage(
      x = sdm,
      colors = "#47ae24"
    )|>
    addRasterImage(
      x = proMask,
      colors = "#746fae"
    )|>
    addLegend(
      position = "topright",
      title = "GRS in situ",
      colors = c("#47ae24","#746fae"),
      labels = c("Distribution","Protected Areas"),
      opacity = 1
    )|>
    addControl(html = map_title, position = "bottomleft")

  # create output data
  output <- list(
    results = df_output ,
    protectAreaMask = proMask,
    map = map
  )
  return(output)
}
