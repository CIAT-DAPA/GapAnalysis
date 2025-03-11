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

ERSin <- function(taxon, sdm, occurrence_Data, protected_Areas, ecoregions, idColumn) {
  # mask protected areas layer
  mask1 <- ifel(test = sdm == 1, yes = 1, no = NA)
  # crop protected areas raster
  p1 <- terra::crop(x = protected_Areas, y = sdm)
  # multiple to create mask
  p1 <- p1 * mask1

  # filter the occurrence data to the species of interest
  d1 <- occurrence_Data |>
    dplyr::filter(species == taxon) |>
    terra::vect(geom=c("longitude", "latitude"))

  # determine the eco regions present in the
  inter <- terra::intersect(x = d1, y = ecoregions) |>
    terra::as.data.frame()
  # select ecoregions of interest
  ecoCodes <- unique(inter[,idColumn])
  # index with selection
  ## conver to table for easier indexing
  eco2 <- terra::as.data.frame(ecoregions)
  ## select
  n1 <- ecoregions[eco2[,idColumn] %in% ecoCodes, ]

  # total number of eco regions within the SDM
  ## ecoregions with predicted presence within the boundaries
  totEco <- terra::zonal(x = sdm , z = n1, fun = "sum",na.rm=TRUE)

  totEco$ecoID <- as.data.frame(n1)[,idColumn]

  # reduce to number of rows for simple math
  totalEcoregions <- totEco |>
    dplyr::filter_at(1, all_vars(. >= 1)) |>
    dplyr::select(ecoID) |>
    dplyr::distinct()|>
    nrow()
  # total number of eco regions within the SDM with protect areas.
  totProEco <- terra::zonal(x = p1 ,z = n1, fun = "sum",na.rm=TRUE)
  # assign the eco id
  totProEco$ecoID <- as.data.frame(n1)[,idColumn]
  # filter to include ecoregions with protected areas
  totalProtectedEcoregions <- totProEco |>
    dplyr::filter_at(1, all_vars(. >= 1)) |>
    dplyr::select(ecoID) |>
    dplyr::distinct()|>
    nrow()

  if(totalProtectedEcoregions == 0){
    ers <- 0
  }else{
    ers <- (totalProtectedEcoregions/totalEcoregions)*100
  }



  df <- dplyr::tibble(Taxon = taxon,
                   "Ecoregions within model" = totalEcoregions,
                   "Ecoregions with protected areas" = totalProtectedEcoregions,
                   "ERS insitu" = ers)
  return(df)
}
