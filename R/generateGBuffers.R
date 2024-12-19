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



generateGBuffers <- function(taxon, occurrence_Data, bufferDistM){

  # filter the occurrence data to the species of interest
  d1 <- occurrence_Data |>
    dplyr::filter(species == taxon & type == "G") |>
    terra::vect(geom=c("longitude", "latitude"))
  terra::crs(d1) <- "epsg:4326"

  if(nrow(d1)>0){
    d2 <- d1 |>
      terra::buffer(width = bufferDistM)
  }else{
    d2 <- "No G points present"
  }

  return(d2)
}
