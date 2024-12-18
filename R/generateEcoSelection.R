#'
#' #' ##Obtaining occurrences from example
#' load("data/CucurbitaData.rda")
#' #' ##Obtaining species names from the data
#' taxon <- CucurbitaData$species[1]
#' #' ##Obtaining ecoregions layer
#' load("data/ecoExample.rda")
#'
#'
#' taxon <- taxon
#' occurrence_Data <- CucurbitaData
#' ecoregions <- terra::vect(eco1)
#' idColumn <- "ECO_ID_U"

generateEcoSelection <- function(taxon, ococcurrence_Data, ecoregions, idColumn){

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

  # return
  return(n1)
}
