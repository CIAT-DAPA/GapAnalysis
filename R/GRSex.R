# ##Obtaining occurrences from example
# load("data/CucurbitaData.rda")
# ##Obtaining Raster_list
# load("data/CucurbitaRasts.rda")
# ##Obtaining protected areas raster
# load("data/protectAreasRast.rda")
#
# library(terra)
# library(dplyr)
# #
# taxon <- CucurbitaData$species[1]
# sdm <- terra::unwrap(CucurbitaRasts)[[1]]
# occurrence_Data <- CucurbitaData
# source("R/generateGBuffers.R")
# gBuffer <- generateGBuffers(taxon = taxon, occurrence_Data = occurrence_Data, bufferDistM =  50000)


GRSex <- function(taxon, sdm, gBuffer) {
  ## all the areas of the cells
  r1 <- cellSize(sdm,unit="km")
  ## mutliple by origin. values of 1 will retain area measures
  r2 <- r1 * sdm
  totalArea <- sum(values(r2), na.rm = TRUE)

  # clause to see if any g points exist
  if(class(gBuffer) == "character"){ # not sure if this is the best condition
    grs <- 0
    gArea <- 0
  }else{
    ## rasterize the object
    b1 <- terra::rasterize(x = gBuffer, y = sdm)
    c2 <- r1 * b1
    gArea <- sum(values(c2), na.rm = TRUE)

    # clause to determine if any of the buffered area falls within predicted area
    if(gArea == 0){
      grs <- 0
      gArea <- 0
    }else{
      #calculate GRS
      grs <- min(c(100, gArea/totalArea*100))
    }
  }

  #create data.frame with output
  out_df <- dplyr::tibble(Taxon=taxon,
                       'Area of model km2'=totalArea,
                       'G buffer areas in model km2' =gArea,
                       "GRS exsitu" =grs)
  return(out_df)
}
