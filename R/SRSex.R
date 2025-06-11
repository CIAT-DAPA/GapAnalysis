
#' Title
#'
#' @param taxon
#' @param occurrence_Data
#'
#' @return
#' @export
#'
#' @examples
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
