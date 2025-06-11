

#' Title
#'
#' @param taxon
#' @param srsin
#' @param grsin
#' @param ersin
#'
#' @return
#' @export
#'
#' @examples
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
