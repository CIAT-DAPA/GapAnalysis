#' @title Select relivent ecoregions
#' @name generateEcoSelection
#' @description
#' Utilizes the occurrence data location to select all ecoregions that intersect with thoses points. Helpful as it
#' reduces the overall file size of the ecoregion object.
#'
#' @param taxon A character object that defines the name of the species as listed in the occurrence dataset
#'
#' @param occurrenceData a data frame of values containing columns for the taxon, latitude, longitude, and type
#'
#' @param ecoregions A terra vect object the contains spatial information on all ecoregions of interests
#'
#' @param idColumn A character vector that notes what column within the ecoregions object should be used as a unique ID
#'
#'
#' @return selectedEcos : a terra vect that contains the selected ecoregion features
#'
#' @examples
#' ##Obtaining occurrences from example
#' load("data/CucurbitaData.rda")
#' ## ecoregion features
#' load("data/ecoExample.rda")
#'
#' # convert the dataset for function
#' taxon <- "Cucurbita_cordata"
#' occurrenceData <- CucurbitaData
#' ecoregions <- terra::vect(eco1)
#'
#' #Running generateEcoSelection
#' selectedEcos <- generateEcoSelection(taxon = Cucurbita_splist,
#'                     occurrenceData = occurrenceData,
#'                     ecoregions = ecoregions,
#'                     idColumn = "ECO_NAME"
#'                     )
#'
#'
#' @references
#' Khoury et al. (2019) Ecological Indicators 98:420-429. doi: 10.1016/j.ecolind.2018.11.016
#' Carver et al. (2021) GapAnalysis: an R package to calculate conservation indicators using spatial information
generateEcoSelection <- function(taxon, occurrenceData, ecoregions, idColumn){

  # filter the occurrence data to the species of interest
  d1 <- occurrenceData |>
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
  selectedEcos <- ecoregions[eco2[,idColumn] %in% ecoCodes, ]

  # return
  return(selectedEcos)
}
