##Obtaining occurrences from example
load("data/CucurbitaData.rda")
##Obtaining Raster_list
load("data/CucurbitaRasts.rda")
##Obtaining protected areas raster
load("data/protectAreasRast.rda")
## ecoregions
load("data/ecoExample.rda")

library(terra)
library(dplyr)
#
taxon <- CucurbitaData$species[1]
sdm <- terra::unwrap(CucurbitaRasts)[[1]]
occurrence_Data <- CucurbitaData
ecoregions <- terra::vect(eco1)
protected_Areas <- terra::unwrap(protectAreasRast)
source("R/generateCounts.R")

# source individual functions
source("R/ERSin.R")
source("R/SRSin.R")
source("R/GRSin.R")

# generate objects
srsin <- SRSin(taxon = taxon, sdm = sdm, occurrence_Data = occurrence_Data, protected_Areas = protected_Areas)
grsin <- GRSin(taxon = taxon, sdm = sdm, protected_Areas = protected_Areas)
ersin <- ERSin(taxon = taxon, sdm = sdm, occurrence_Data = occurrence_Data, protected_Areas = protected_Areas,
               ecoregions = ecoregions, idColumn = "ECO_ID_U")


FCSin <- function(taxon, srsin, grsin, ersin){
  # define variables
  srs <- srsin$SRS.insitu
  ers <- grsin$GRS.insitu
  grs <- ersin$ERS.insitu


  # calculate the mean across the three measures
  sp_fcs <- mean(c(srs,grs,ers), na.rm=T)

  out_df <- data.frame(Taxon=taxon,
                       "SRS insitu" = srs,
                       "GRS insitu"= grs,
                       "ERS insitu" = ers,
                       "FCS insitu" = sp_fcs,
                       "FCS insitu score" = NA)

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

  out_df$FCS_Score <- score
  return(out_df)
}
