
#' Title
#'
#' @param taxon
#' @param fcsin
#' @param fcsex
#'
#' @return
#' @export
#'
#' @examples
FCSc_mean <- function(taxon, fcsin, fcsex) {

  #compute FCSc_min and FCSc_max
  data_comb <- dplyr::tibble(Taxon = taxon,
                          "FCS exsitu" = fcsex$`FCS exsitu`,
                          "FCS insitu" = fcsin$`FCS insitu`)


  data_comb$FCSc_min <- min(c(data_comb$`FCS exsitu`,data_comb$`FCS insitu`),na.rm=T)
  data_comb$FCSc_max <- max(c(data_comb$`FCS exsitu`,data_comb$`FCS insitu`),na.rm=T)
  data_comb$FCSc_mean <- mean(c(data_comb$`FCS exsitu`,data_comb$`FCS insitu`),na.rm=T)

  #assign classes (min)
  if (data_comb$FCSc_min < 25) {
    data_comb$FCSc_min_class <- "UP"
  } else if (data_comb$FCSc_min >= 25 & data_comb$FCSc_min < 50) {
    data_comb$FCSc_min_class <- "HP"
  } else if (data_comb$FCSc_min >= 50 & data_comb$FCSc_min < 75) {
    data_comb$FCSc_min_class <- "MP"
  } else {
    data_comb$FCSc_min_class <- "LP"
  }

  #assign classes (max)
  if (data_comb$FCSc_max < 25) {
    data_comb$FCSc_max_class <- "UP"
  } else if (data_comb$FCSc_max >= 25 & data_comb$FCSc_max < 50) {
    data_comb$FCSc_max_class <- "HP"
  } else if (data_comb$FCSc_max >= 50 & data_comb$FCSc_max < 75) {
    data_comb$FCSc_max_class <- "MP"
  } else {
    data_comb$FCSc_max_class <- "LP"
  }

  #assign classes (mean)
  if (data_comb$FCSc_mean < 25) {
    data_comb$FCSc_mean_class <- "UP"
  } else if (data_comb$FCSc_mean >= 25 & data_comb$FCSc_mean < 50) {
    data_comb$FCSc_mean_class <- "HP"
  } else if (data_comb$FCSc_mean >= 50 & data_comb$FCSc_mean < 75) {
    data_comb$FCSc_mean_class <- "MP"
  } else {
    data_comb$FCSc_mean_class <- "LP"
  }

  return(data_comb)
}
