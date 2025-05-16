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
  vals <- terra::extract(sdm, d1)
  # points in sdm
  p1 <- d1[vals[,2] == 1, ]
  # extract vals from protected layers
  pro1 <- terra::extract(protectedAreas, p1)
  # points in pro
  protectedPoints <- p1[pro1[,2] == 1, ]
  p1$protected <- pro1[,2]

  # srsin
  srsin <- nrow(protectedPoints)/nrow(p1) *100

  # dataframe for export
  out_df <- dplyr::tibble(Taxon = taxon,
                          "Total Observations" = nrow(d1),
                          "Total records in SDM" = nrow(p1),
                          "Records in Protected areas" = nrow(protectedPoints),
                          "SRS insitu" = srsin)
  # quick map
  p1$color <- ifelse(is.na(p1$protected), "#00000040", "#746fae")

  # 3. Create the plot using terra::plot()
  # cropPro <- terra::mask(protectedAreas, sdm)
  # terra::plot(cropSDM, main = "Points within SDM inside of protected areas", xlab = "Longitude", ylab = "Latitude")
  # terra::plot(p1, col = p1$color, pch = 16, cex = 2, add = TRUE)
  # terra::add_legend("bottomright", legend = c("Protected", "Not Protected"),
  #               pch = 16, col = c("red", "blue"), cex = 0.75)

  map_title <- "<h3 style='text-align:center; background-color:rgba(255,255,255,0.7); padding:2px;'>Points within SDM inside of protected areas</h3>"
  map <- leaflet() |>
    addTiles() |>
    # addPolygons(data = ecoSelect,
    #             color = "#444444",
    #             weight = 1,
    #             opacity = 1.0,
    #             fillOpacity = 0.1,
    #             popup = ~ECO_NAME,
    #             fillColor = NA)|>
    # addPolygons(data = gapEcos,
    #             color = "#444444",
    #             weight = 1,
    #             opacity = 1.0,
    #             popup = ~ECO_NAME,
    #             fillOpacity = 0.5,
    #             fillColor = "#f0a01f")|>
    addRasterImage(
      x = sdm,
      colors = "#47ae24"
    )|>
    # addRasterImage(
    #   x = cropPro,
    #   colors = "#746fae"
    # )|>
    addCircleMarkers(
      data = p1,
      color = ~color,
      radius = 1,
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

