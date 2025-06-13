#' @title Final conservation score ex situ
#' @name FCSex
#' @description This function calculates the average of the three ex situ conservation metrics
#'   returning a final conservation score summary table. It also assigns conservation priority categories
#' @param taxon
#'
#' @param srsex
#' @param grsex
#' @param ersex
#'
#' @return
#'
#' @examples
#'
#'
#' @references
#' Khoury et al. (2019) Ecological Indicators 98:420-429. doi: 10.1016/j.ecolind.2018.11.016
#' Carver et al. (2021) GapAnalysis: an R package to calculate conservation indicators using spatial information
FCSex <- function(taxon, srsex, grsex, ersex){

  # calculate the mean across the three measures
  srs <- srsex$`SRS exsitu`
  grs <- grsex$results$`GRS exsitu`
  ers <- ersex$results$`ERS exsitu`

  # generate the mean exsitu score
  sp_fcs <- mean(c(srs,
                   ers,
                   grs), na.rm=T)

  out_df <- dplyr::tibble(Taxon = taxon,
                         "SRS exsitu"= srs,
                         "GRS exsitu" = grs,
                         "ERS exsitu"= ers,
                         "FCS exsitu"=sp_fcs,
                         "FCS existu score" = NA)

  #assign classes (min)
  if (sp_fcs < 25) {
    score <- "UP"
  } else if (sp_fcs >= 25 & sp_fcs < 50) {
    score <- "HP"
  } else if (sp_fcs >= 50 & sp_fcs < 75) {
    score <- "MP"
  } else {
    score <- "LP"
  }
  out_df$"FCS existu score" <- score
  return(out_df)

}
