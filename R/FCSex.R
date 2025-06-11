
#' Title
#'
#' @param taxon
#' @param srsex
#' @param grsex
#' @param ersex
#'
#' @return
#' @export
#'
#' @examples
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
