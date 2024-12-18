#
# ##Obtaining occurrences from example
# load("data/CucurbitaData.rda")
# ##Obtaining species names from the data
# taxon <- CucurbitaData$species[1]
#
# taxon <- taxon
# occurrence_Data <- CucurbitaData
# bufferDistKM <- 50000
#

generateGBuffers <- function(taxon, ococcurrence_Data, bufferDistM){

  # filter the occurrence data to the species of interest
  d1 <- occurrence_Data |>
    dplyr::filter(species == taxon & type == "G") |>
    terra::vect(geom=c("longitude", "latitude"))|>
    terra::buffer(width = bufferDistKM)
  return(d1)
}
