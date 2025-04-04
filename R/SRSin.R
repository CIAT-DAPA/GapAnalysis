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


  # srsin
  srsin <- nrow(protectedPoints)/nrow(p1) *100

  # dataframe for export
  out_df <- dplyr::tibble(Taxon = taxon,
                          "Total Observations" = nrow(d1),
                          "Total records in SDM" = nrow(p1),
                          "Records in Protected areas" = nrow(protectedPoints),
                          "SRS insitu" = srsin)
  # quick map
  p1$color <- ifelse(is.na(pro1[,2]), "blue", "red")
  p1$inProtectedArea <- ifelse(is.na(pro1[,2]), "No", "Yes")

  # 3. Create the plot using terra::plot()
  cropSDM <- terra::crop(sdm, d1)
  terra::plot(cropSDM, main = "Points within SDM inside of protected areas", xlab = "Longitude", ylab = "Latitude")
  terra::plot(p1, col = p1$color, pch = 16, cex = 2, add = TRUE)
  terra::add_legend("bottomright", legend = c("Protected", "Not Protected"),
                pch = 16, col = c("red", "blue"), cex = 0.75)

  # define output
  output <- list(
    results = out_df,
    points = p1
    )
  return(output)
}

