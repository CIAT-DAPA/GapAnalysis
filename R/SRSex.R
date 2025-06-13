
#' @title Sampling representativeness score ex situ
#' @name SRSex
#' @description The SRSex process provides a general indication of the completeness of ex situ conservation collections,
#'  calculating the ratio of germplasm accessions (G) available in ex situ repositories to reference (H) records for each taxon,
#'  making use of all compiled records, regardless of whether they include coordinates, with an ideal (i.e., comprehensive) conservation
#'  ratio of 1:1. In this and in the subsequent measurements, if no G or H records exist, taxa are automatically considered
#'  to be of high priority for further conservation action and assigned a value of 0. If there are more G than H records,
#' SRSex is set to 100.
#' @param taxon
#'
#' @param occurrence_Data
#'
#' @return
#'
#' @examples
#'
#'
#' @references
#' Khoury et al. (2019) Ecological Indicators 98:420-429. doi: 10.1016/j.ecolind.2018.11.016
#' Carver et al. (2021) GapAnalysis: an R package to calculate conservation indicators using spatial information
SRSex <- function(taxon, occurrence_Data) {
  # generarte the counts data for species
  sp_counts <- generateCounts(taxon = taxon,
                       occurrence_Data = occurrence_Data)
  # caluse for no h points
  if(sp_counts$totalGRecords >= 1 & sp_counts$totalHRecords == 0){
    srs <-100
  }

  #clause for no data
  if (sp_counts$totalGRecords == 0 & sp_counts$totalHRecords ==0) {
    srs <- 0
  } else {
    # clause for species with data
    srs <- min(c(100,sp_counts$totalGRecords/sp_counts$totalHRecords*100))
  }


  #create data.frame with output
  out_df <- dplyr::tibble(Taxon=sp_counts$species,
                       "Total records"=sp_counts$totalRecords,
                       "Total with cooordinates"=sp_counts$totalUseful,
                       "Total G records"= sp_counts$totalGRecords,
                       "G records with coordinates"=sp_counts$totalGUseful,
                       "Total H records"=sp_counts$totalHRecords,
                       "H records with coordinates"=sp_counts$totalHUseful,
                       "SRS exsitu" =srs)
  return(out_df)
}
