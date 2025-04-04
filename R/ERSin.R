#
# ##Obtaining occurrences from example
# load("data/CucurbitaData.rda")
# ##Obtaining Raster_list
# load("data/CucurbitaRasts.rda")
# ##Obtaining protected areas raster
# load("data/protectAreasRast.rda")
# load("data/ecoExample.rda")
#
# taxon <- CucurbitaData$species[1]
# sdm <- terra::unwrap(CucurbitaRasts)$cordata
# occurrence_Data <- CucurbitaData
# ecoregions <- terra::vect(eco1)
# idColumn <- "ECO_ID_U"
# protected_Areas <- terra::unwrap(protectAreasRast)
#
# library(terra)
# library(dplyr)
#
# ERSin(taxon, sdm, occurrence_Data, protected_Areas, ecoregions, idColumn)

ERSin <- function(taxon, sdm, occurrence_Data, protectedAreas, ecoregions, idColumn) {
  # crop protected areas to sdm
  pro <- terra::crop(protectedAreas, sdm)
  # mask to model
  proMask <- pro * sdm
  # crop ecos to sdm
  eco <- terra::crop(ecoregions, sdm)

  # Get ecoregions in sdm
  totEco <- terra::zonal(x = sdm , z = eco, fun = "sum",na.rm=TRUE) |> pull()
  nEcoModel <- length(totEco[!is.na(totEco)])
  # Get ecoregions in pro areas
  totPro <- terra::zonal(x = sdm , z = eco, fun = "sum",na.rm=TRUE) |> pull()
  nProModel <- length(totPro[!is.na(totPro)])
  # add the eco results to ecoregion layer
  eco$inDist <- totEco
  eco$protected <- totPro
  # filter spatial object for output
  eco1 <- eco[!is.na(eco$inDist), ]
  eco1 <- eco1[is.na(eco1$protected), ]
  # calculate the ers
  if(nProModel == 0){
    ers <- 0
  }else{
    ers <- (nProModel/nEcoModel)*100
  }
  #results
  df <- dplyr::tibble(Taxon = taxon,
                   "Ecoregions within model" = nEcoModel,
                   "Ecoregions with protected areas" = nProModel,
                   "ERS insitu" = ers)
  # plot missing ecos
  terra::plot(sdm, col = "yellow",
              main = "Ecoregions within the SDM without Protected Area",
              xlab = "Longitude", ylab = "Latitude")
  if(nrow(eco1) >0 ){
    terra::plot(eco1, add = TRUE)
  }

  # output
  output = list(
    results = df,
    missingEcos = eco1
  )
  return(output)
}
