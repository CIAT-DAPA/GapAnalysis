#' @title Download datasets from Dataverse
#' @name GetDataSets
#' @description
#' A short description...
#'
#' @param
#'
#' @return
#'
#' @examples
#'
#'
#' @references
#' Khoury et al. (2019) Ecological Indicators 98:420-429. doi: 10.1016/j.ecolind.2018.11.016
#' Carver et al. (2021) GapAnalysis: an R package to calculate conservation indicators using spatial information
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
