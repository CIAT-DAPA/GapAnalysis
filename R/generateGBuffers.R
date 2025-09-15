#' @title Generate buffer of G type occurrences
#' @name generateGBuffers
#' @description
#' Produces a terra vect object representing the area around the G type occurrences
#'
#' @param taxon A character object that defines the name of the species as listed in the occurrence dataset
#' @param occurrenceData a data frame of values containing columns for the taxon, latitude, longitude, and type
#' @param bufferDistM Distance in meters. Used to set the size of the buffered objects.
#'
#' @return A list object containing
#' 1. data : a terra vect object showing all the buffered areas around the G type occurrences
#' 2. map : a leaflet object showing the spatial results of the function
#'

#' @examples
#' ##Obtaining occurrences from example
#' data(CucurbitaData)
#'
#' # convert the dataset for function
#' taxon <- "Cucurbita_cordata"
#' occurrenceData <- CucurbitaData
#'
#' #Running generateGBuffers
#' gBuffer <- generateGBuffers(taxon = taxon,
#'                     occurrenceData = occurrenceData,
#'                     bufferDistM = 50000
#'                     )
#'
#'
#'
#' @references
#' Khoury et al. (2019) Ecological Indicators 98:420-429. doi: 10.1016/j.ecolind.2018.11.016
#' Carver et al. (2021) GapAnalysis: an R package to calculate conservation indicators using spatial information
#' @importFrom dplyr filter
#' @importFrom terra vect crs buffer
#' @importFrom leaflet addTiles addPolygons addLegend addRasterImage addCircleMarkers
#' @export

generateGBuffers <- function(taxon, occurrenceData, bufferDistM){

  # filter the occurrence data to the species of interest
  d1 <- occurrenceData |>
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
  map <- leaflet::leaflet(d2) |>
    leaflet::addTiles() |>
    leaflet::addPolygons(color = "#444444",
                weight = 1,
                opacity = 1.0,
                fillOpacity = 0.5,
                fillColor = "#6300f0")|>
    leaflet::addCircleMarkers(
      data = d1,
      color = "#000",
      radius = 2,
      opacity = 1
    )|>
    leaflet::addControl(html = map_title, position = "bottomleft")


  return(list(
    data = d2,
    map = map
  ))
}
