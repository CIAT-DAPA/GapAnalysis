#'
#' ##Obtaining occurrences from example
#' load("data/CucurbitaData.rda")
#' ##Obtaining species names from the data
#' taxon <- CucurbitaData$species[1]
#' ##Obtaining Raster_list
#' load("data/CucurbitaRasts.rda")
#' ##Obtaining protected areas raster
#' load("data/protectAreasRast.rda")
#' #' #Running SRSin

SRSin <- function(taxon, sdm, occurrence_Data,  protectedAreas){
  # remove all points not inside of the sdm
  # then test those for presence in protected areas
  # return the proportion and the visualization of points


  # filter the occurrence data to the species of interest
  d1 <- occurrence_Data |>
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

