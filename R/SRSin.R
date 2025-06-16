
#' @title Sampling representativeness score in situ
#' @name SRSin
#' @description The SRSin process calculates the proportion of all occurrences of a taxon falling within
#' the distribution model that also fall within a protected area
#'
#' @param taxon A character object that defines the name of the species as listed in the occurrence dataset
#'
#'
#' @param sdm a terra rast object that
#'
#' @param occurrenceData a data frame of values containing columns for the taxon, latitude, longitude, and type
#'
#' @param protectedAreas A terra rast object the contian spatial location of protected areas.
#'
#' @return A list object containing
#' 1. results : a data frames of values summarizing the results of the function
#' 2. points : a terra vect object showing all the points present within protected areas
#' 3. map : a leaflet object showing the spatial results of the function
#'
#' @examples
#' ##Obtaining occurrences from example
#' load("data/CucurbitaData.rda")
#' ##Obtaining Raster_list
#' load("data/CucurbitaRasts.rda")
#' ##Obtaining protected areas raster
#' load("data/protectAreasRast.rda")
#'
#' # convert the dataset for function
#' taxon <- "Cucurbita_cordata"
#' sdm <- terra::unwrap(CucurbitaRasts)$cordata
#' occurrenceData <- CucurbitaData
#' protectedAreas <- terra::unwrap(protectArea)
#' #Running SRSin
#' srs_insitu <- SRSin(taxon = Cucurbita_splist,
#'                     sdm = CucurbitaData,
#'                     occurrenceData = CucurbitaData,
#'                     protectedAreas = ProtectedAreas
#'                     )
#'
#' @references
#' Khoury et al. (2019) Ecological Indicators 98:420-429. doi: 10.1016/j.ecolind.2018.11.016
#' Carver et al. (2021) GapAnalysis: an R package to calculate conservation indicators using spatial information
SRSin <- function(taxon, sdm, occurrenceData,  protectedAreas){
  # remove all points not inside of the sdm
  # then test those for presence in protected areas
  # return the proportion and the visualization of points


  # filter the occurrence data to the species of interest
  d1 <- occurrenceData |>
    dplyr::filter(species == taxon) |>
    terra::vect(geom=c("longitude", "latitude"), crs="+proj=longlat +datum=WGS84")
  # extract values from the sdm
  d1$inSDM <- terra::extract(sdm, d1, ID = FALSE)
  # points in sdm
  p1 <- d1[d1$inSDM == 1, ]
  # extract vals from protected layers
  p1$inPro <- terra::extract(protectedAreas, p1, ID=FALSE)
  # points in pro
  protectedPoints <- p1[p1$inPro== 1, ]

  # srsin
  srsin <- nrow(protectedPoints)/nrow(p1) *100

  # dataframe for export
  out_df <- dplyr::tibble(Taxon = taxon,
                          "Total Observations" = nrow(d1),
                          "Total records in SDM" = nrow(p1),
                          "Records in Protected areas" = nrow(protectedPoints),
                          "SRS insitu" = srsin)
  # quick map
  p1$color <- ifelse(is.na(p1$inPro), "#444444", "#746fae")

  map_title <- "<h3 style='text-align:center; background-color:rgba(255,255,255,0.7); padding:2px;'>Points within SDM inside of protected areas</h3>"
  map <- leaflet() |>
    addTiles() |>
    addRasterImage(
      x = sdm,
      colors = "#47ae24"
    )|>
    addCircleMarkers(
      data = p1,
      color = ~color,
      radius = 1,
      opacity = 1
    )|>
    addLegend(
      position = "topright",
      title = "SRS in situ",
      colors = c("#47ae24","#746fae", "#444444"),
      labels = c("Distribution","Protected Occurrences", "Non Protected Occurrences"),
      opacity = 1
    )|>
    addControl(html = map_title, position = "bottomleft")



  # define output
  output <- list(
    results = out_df,
    points = p1,
    map = map
    )
  return(output)
}

