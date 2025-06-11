
#' Title
#'
#' @param taxon
#' @param ococcurrence_Data
#' @param ecoregions
#' @param idColumn
#'
#' @return
#' @export
#'
#' @examples
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
