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
source("R/ERSex.R")
source("R/SRSex.R")
source("R/GRSex.R")
source("R/generateGBuffers.R")
source("R/generateCounts.R")
gBuffer <- generateGBuffers(taxon = taxon, occurrence_Data = occurrence_Data, bufferDistM =  50000)

# generate objects
srsin <- SRSin(taxon = taxon, sdm = sdm, occurrence_Data = occurrence_Data, protected_Areas = protected_Areas)
grsin <- GRSin(taxon = taxon, sdm = sdm, protected_Areas = protected_Areas)
ersin <- ERSin(taxon = taxon, sdm = sdm, occurrence_Data = occurrence_Data, protected_Areas = protected_Areas,
               ecoregions = ecoregions, idColumn = "ECO_ID_U")
fcsin <- FCSin(taxon = taxon, srsin = srsin, grsin = grsin, ersin = ersin)

# generate objects
srsex <- SRSex(taxon = taxon, occurrence_Data = occurrence_Data)
grsex <- GRSex(taxon = taxon, sdm = sdm, gBuffer = gBuffer)
ersex <- ERSex(taxon = taxon, sdm = sdm, occurrence_Data = occurrence_Data, gBuffer = gBuffer,
               ecoregions = ecoregions, idColumn = "ECO_ID_U")
fcsex <- FCSex(taxon = taxon, srsex = srsex, grsex = grsex, ersex = ersex)








FCSc_mean <- function(taxon, fcsin, fcsex) {

  #compute FCSc_min and FCSc_max
  data_comb <- data.frame(Taxon = taxon,
                          "FCS exsitu" = fcsex$FCS.exsitu,
                          "FCS insitu" = fcsin$FCS.insitu)


  data_comb$FCSc_min <- min(c(data_comb$FCS.exsitu,data_comb$FCS.insitu),na.rm=T)
  data_comb$FCSc_max <- max(c(data_comb$FCS.exsitu,data_comb$FCS.insitu),na.rm=T)
  data_comb$FCSc_mean <- mean(c(data_comb$FCS.exsitu,data_comb$FCS.insitu),na.rm=T)

  #assign classes (min)
  if (data_comb$FCSc_min < 25) {
    data_comb$FCSc_min_class <- "UP"
  } else if (data_comb$FCSc_min >= 25 & data_comb$FCSc_min < 50) {
    data_comb$FCSc_min_class <- "HP"
  } else if (data_comb$FCSc_min >= 50 & data_comb$FCSc_min < 75) {
    data_comb$FCSc_min_class <- "MP"
  } else {
    data_comb$FCSc_min_class <- "LP"
  }

  #assign classes (max)
  if (data_comb$FCSc_max < 25) {
    data_comb$FCSc_max_class <- "UP"
  } else if (data_comb$FCSc_max >= 25 & data_comb$FCSc_max < 50) {
    data_comb$FCSc_max_class <- "HP"
  } else if (data_comb$FCSc_max >= 50 & data_comb$FCSc_max < 75) {
    data_comb$FCSc_max_class <- "MP"
  } else {
    data_comb$FCSc_max_class <- "LP"
  }

  #assign classes (mean)
  if (data_comb$FCSc_mean < 25) {
    data_comb$FCSc_mean_class <- "UP"
  } else if (data_comb$FCSc_mean >= 25 & data_comb$FCSc_mean < 50) {
    data_comb$FCSc_mean_class <- "HP"
  } else if (data_comb$FCSc_mean >= 50 & data_comb$FCSc_mean < 75) {
    data_comb$FCSc_mean_class <- "MP"
  } else {
    data_comb$FCSc_mean_class <- "LP"
  }

  return(data_comb)
}
