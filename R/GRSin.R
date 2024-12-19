# ##Obtaining occurrences from example
# load("data/CucurbitaData.rda")
# ##Obtaining Raster_list
# load("data/CucurbitaRasts.rda")
# ##Obtaining protected areas raster
# load("data/protectAreasRast.rda")
#
# library(terra)
# library(dplyr)
#
# taxon <- CucurbitaData$species[1]
# sdm <- terra::unwrap(CucurbitaRasts)[[1]]
# occurrence_Data <- CucurbitaData
# protected_Areas <- terra::unwrap(protectAreasRast)

GRSin <- function(taxon, sdm, protected_Areas = NULL){
  # mask protected areas layer
  sdm[sdm == 0] <- NA
  # crop protected areas raster
  p1 <- terra::crop(x = protected_Areas, y = sdm)
  # multiple to create mask
  ## areas both in protected area and sdm
  p2 <- p1 * sdm

  protectArea <- terra::cellSize(p2,mask = TRUE,unit = "km" )
  protectAreaSum <- sum(values(protectArea), na.rm = TRUE)

  thresholdArea <- terra::cellSize(sdm, mask = TRUE, unit = "km")
  thresholdAreaSum <- sum(values(thresholdArea), na.rm = TRUE)

  # calcualte the total area
  if(protectAreaSum == 0){
    protectAreaSum <- 0
    grs <- 0
  }else{
    grs <- min(c(100, protectAreaSum/thresholdAreaSum*100))
  }
  # return objects
  df <- data.frame(Taxon = taxon,
                   'Area of model km2' = round(thresholdAreaSum, digits = 0),
                   'Area in protected ares km2' = round(protectAreaSum, digits = 0),
                   "GRS insitu" = grs)
  return(df)
}
