pacman::p_load(dplyr, terra, sf)

##Obtaining occurrences from example
load("data/CucurbitaData.rda")
##Obtaining Raster_list
load("data/CucurbitaRasts.rda")
##Obtaining protected areas raster
load("data/protectAreasRast.rda")
## ecoregions
load("data/ecoExample.rda")


# prep for the function
taxon <- CucurbitaData$species[1]
sdm <- terra::unwrap(CucurbitaRasts)[[1]]
occurrence_Data <- CucurbitaData
ecoregions <- terra::vect(eco1)
source("R/generateGBuffers.R")
source("R/generateCounts.R")
gBuffer <- generateGBuffers(taxon = taxon,
                            occurrence_Data = occurrence_Data,
                            bufferDistM =  50000)

# source individual functions
source("R/ERSex.R")
source("R/SRSex.R")
source("R/GRSex.R")

# generate objects
srsex <- SRSex(taxon = taxon,
               occurrence_Data = occurrence_Data)
grsex <- GRSex(taxon = taxon,
               sdm = sdm,
               gBuffer = gBuffer)
ersex <- ERSex(taxon = taxon,
               sdm = sdm,
               occurrence_Data = occurrence_Data, gBuffer = gBuffer,
               ecoregions = ecoregions, idColumn = "ECO_ID_U")
source("R/FCSex.R")

fcsex <- FCSex(taxon = taxon,
               srsex = srsex,
               grsex = grsex,
               ersex = ERSex(taxon = taxon,
                             sdm = sdm,
                             occurrence_Data = occurrence_Data, gBuffer = gBuffer,
                             ecoregions = ecoregions, idColumn = "ECO_ID_U"))

# insitu
source("R/ERSin.R")
source("R/SRSin.R")
source("R/GRSin.R")

# protect areas
protectAreasRast <- terra::unwrap(protectAreasRast)

ersin <- ERSin(taxon = taxon,
               sdm = sdm,
               occurrence_Data = occurrence_Data,
               protected_Areas = protectAreasRast,
               ecoregions = ecoregions,
               idColumn = "ECO_ID_U")


# next steps for test  ----------------------------------------------------

## work up single species workflow

## test out the for loop implementation
for(taxon in taxa){
  # assign the taxon and data
}


## testing out the purrr implementation
purrr::map2(.x = speciesList,
            .y = sdms,
            .f = ERSin,
           occurrence_Data = occurrence_Data,
           protected_Areas = protectAreasRast,
           ecoregions = ecoregions,
           idColumn = "ECO_ID_U")



