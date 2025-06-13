#' @title Download datasets from Dataverse
#' @name GetDataSets
#' @description
#' A short description...
#'
#' @param
#'
#' @return
#'
#' @examples
#'
#'
#' @references
#' Khoury et al. (2019) Ecological Indicators 98:420-429. doi: 10.1016/j.ecolind.2018.11.016
#' Carver et al. (2021) GapAnalysis: an R package to calculate conservation indicators using spatial information
generateGBuffers <- function(taxon, occurrence_Data, bufferDistM){

  # filter the occurrence data to the species of interest
  d1 <- occurrence_Data |>
    dplyr::filter(species == taxon & type == "G") |>
    terra::vect(geom=c("longitude", "latitude"))
  terra::crs(d1) <- "epsg:4326"

  if(nrow(d1)>0){
    d2 <- d1 |>
      terra::buffer(width = bufferDistM)
  }else{
    d2 <- "No G points present"
  }
  # # generate a plot of the points for a quality check)
  # terra::plot(d2,  main = "Buffer G points",
  #             xlab = "Longitude", ylab = "Latitude")
  # # add points as reference
  # terra::plot(d1, add = TRUE)

  # leaflet map of
  map_title <- "<h3 style='text-align:center; background-color:rgba(255,255,255,0.7); padding:2px;'>Buffered G Occurrences</h3>"
  map <- leaflet(d2) |>
    addTiles() |>
    addPolygons(color = "#444444",
                weight = 1,
                opacity = 1.0,
                fillOpacity = 0.5,
                fillColor = "#6300f0")|>
    addCircleMarkers(
      data = d1,
      color = "#000",
      radius = 2,
      opacity = 1
    )|>
    addControl(html = map_title, position = "bottomleft")


  return(list(
    data = d2,
    map = map
  ))
}
