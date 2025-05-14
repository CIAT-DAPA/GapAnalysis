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
  # map of protected areas within model extent
  color_palette1 <- colorRampPalette(c("yellow"))(256)
  color_palette2 <- colorRampPalette(c("darkgreen"))(256)
  terra::plot(sdm, col = color_palette1,
              main = "Protect areas within the SDM",
              xlab = "Longitude", ylab = "Latitude")
  terra::plot(proMask, col = color_palette2, add = TRUE)
  terra::add_legend("bottomright", legend = c("SDM", "Protected"),
                                       pch = 16, col = c("yellow", "darkgreen"), cex = 1)
  # create output data
  output <- list(
    results = df_output ,
    data = proMask
  )
  return(output)
}
