#' @title Final Conservation Score measure
#' @name FCSc_mean
#' @description
#' Compiles all tabular data from the individual metrics and generate the final results
#'
#' @param taxon A character object that defines the name of the species as listed in the occurrence dataset
#' @param fcsin A data frame containing summary results from the fcsin function
#' @param fcsex A data frame containing summary results from the fcsex function
#'
#' @return data_comb : a data frame which aggreates final result summaries
#'
#' @examples
#' ##Obtaining occurrences from example
#' data(CucurbitaData)
#' ##Obtaining Raster_list
#' data(CucurbitaRasts)
#' ##Obtaining protected areas raster
#' data(ProtectedAreas)
#' ## ecoregion features
#' data(ecoregions)
#'
#' # convert the dataset for function
#' taxon <- "Cucurbita_cordata"
#' sdm <- terra::unwrap(CucurbitaRasts)$cordata
#' occurrenceData <- CucurbitaData
#' protectedAreas <- terra::unwrap(ProtectedAreas)
#' ecoregions <- terra::vect(ecoregions)
#'
#'
#' # generate exsitu conservation summaries
#' srs_exsitu <- SRSex(taxon = taxon,
#'                     occurrence_Data  = CucurbitaData
#'                     )
#'
#' gBuffer <- generateGBuffers(taxon = taxon,
#'                     occurrenceData = occurrenceData,
#'                     bufferDistM = 50000
#'                     )#'
#'
#' grs_exsitu <- GRSex(taxon = taxon,
#'                     sdm = sdm,
#'                     gBuffer = gBuffer
#'                     )
#'
#' ers_exsitu <- ERSex(taxon = taxon,
#'                     sdm = sdm,
#'                     occurrence_Data = occurrenceData,
#'                     gBuffer = gBuffer,
#'                     ecoregions = ecoregions,
#'                     idColumn = "ECO_NAME"
#'                     )
#'
#' #Running fcsex
#' fcs_exsitu <- FCSex(taxon = taxon,
#'                     srsex = srs_exsitu,
#'                     grsex = grs_exsitu,
#'                     ersex = ers_exsitu
#'                     )
#'
#'
#'
#' # generate insitu conservation summaries
#' srs_insitu <- SRSin(taxon = taxon,
#'                     sdm = sdm,
#'                     occurrenceData = CucurbitaData,
#'                     protectedAreas = protectedAreas
#'                     )
#'
#' grs_insitu <- GRSin(taxon = taxon,
#'                     sdm = sdm,
#'                     protectedAreas = protectedAreas
#'                     )
#'
#' ers_insitu <- ERSin(taxon = taxon,
#'                     sdm = sdm,
#'                     occurrenceData = occurrenceData,
#'                     protectedAreas = protectedAreas,
#'                     ecoregions = ecoregions,
#'                     idColumn = "ECO_NAME"
#'                     )
#'
#' #Running fcsin
#' fcs_insitu <- FCSin(taxon = taxon,
#'                     srsin = srs_insitu,
#'                     grsin = grs_insitu,
#'                     ersin = ers_insitu
#'                     )
#'
#' fsc_combine <- FCSc_mean(taxon = taxon,
#'                          fcsin = fcs_insitu,
#'                          fcsex = fcs_exsitu)
#'
#' @references
#' Khoury et al. (2019) Ecological Indicators 98:420-429. doi: 10.1016/j.ecolind.2018.11.016
#' Carver et al. (2021) GapAnalysis: an R package to calculate conservation indicators using spatial information
#' @importFrom dplyr tibble
#' @export

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
