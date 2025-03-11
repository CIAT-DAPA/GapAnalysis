# ##Obtaining occurrences from example
# load("data/CucurbitaData.rda")
# ##Obtaining Raster_list
# load("data/CucurbitaRasts.rda")
# ##Obtaining protected areas raster
# load("data/protectAreasRast.rda")
# ## ecoregions
# load("data/ecoExample.rda")
#
# library(terra)
# library(dplyr)
#
# # prep for the function
# taxon <- CucurbitaData$species[1]
# sdm <- terra::unwrap(CucurbitaRasts)[[1]]
# occurrence_Data <- CucurbitaData
# ecoregions <- terra::vect(eco1)
# source("R/generateGBuffers.R")
# source("R/generateCounts.R")
# gBuffer <- generateGBuffers(taxon = taxon, occurrence_Data = occurrence_Data, bufferDistM =  50000)
#
# # source individual functions
# source("R/ERSex.R")
# source("R/SRSex.R")
# source("R/GRSex.R")
#
# # generate objects
# srsex <- SRSex(taxon = taxon, occurrence_Data = occurrence_Data)
# grsex <- GRSex(taxon = taxon, sdm = sdm, gBuffer = gBuffer)
# ersex <- ERSex(taxon = taxon, sdm = sdm, occurrence_Data = occurrence_Data, gBuffer = gBuffer,
#                ecoregions = ecoregions, idColumn = "ECO_ID_U")


FCSex <- function(taxon,srsex, grsex, ersex){

  # calculate the mean across the three measures
  srs <- srsex$`SRS exsitu`
  ers <- grsex$`GRS exsitu`
  grs <- ersex$`ERS exsitu`

  # generate the mean exsitu score
  sp_fcs <- mean(c(srs,
                   ers,
                   grs), na.rm=T)

  out_df <- tibble(Taxon = taxon,
                         "SRS exsitu"= srs,
                         "GRS exsitu" = grs,
                         "ERS exsitu"= ers,
                         "FCS exsitu"=sp_fcs,
                         "FCS existu score" = NA)

  #assign classes (min)
  if (sp_fcs < 25) {
    score <- "UP"
  } else if (sp_fcs >= 25 & sp_fcs < 50) {
    score <- "HP"
  } else if (sp_fcs >= 50 & sp_fcs < 75) {
    score <- "MP"
  } else {
    score <- "LP"
  }
  out_df$"FCS existu score" <- score
  return(out_df)

}


# FCSex(taxon = taxon,srsex = srsex, grsex = grsex, ersex = ersex)

