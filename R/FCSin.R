
#'
#' @title Final conservation score in situ
#' @name FCSin
#' @description This function calculates the average of the three in situ conservation metrics and
#' assigns a priority category based on the results
#'
#' @param taxon
#'
#' @param srsin
#' @param grsin
#' @param ersin
#'
#' @return
#'
#' @examples
#'
#'
#' @references
#' Khoury et al. (2019) Ecological Indicators 98:420-429. doi: 10.1016/j.ecolind.2018.11.016
#' Carver et al. (2021) GapAnalysis: an R package to calculate conservation indicators using spatial information
FCSin <- function(taxon, srsin, grsin, ersin){
  # define variables
  srs <- srsin$results$`SRS insitu`
  grs <- grsin$results$`GRS insitu`
  ers <- ersin$results$`ERS insitu`


  # calculate the mean across the three measures
  sp_fcs <- mean(c(srs,grs,ers), na.rm=T)

  out_df <- dplyr::tibble(Taxon=taxon,
                       "SRS insitu" = srs,
                       "GRS insitu"= grs,
                       "ERS insitu" = ers,
                       "FCS insitu" = sp_fcs,
                       "FCS insitu score" = NA)

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

  out_df$`FCS insitu score` <- score
  return(out_df)
}
