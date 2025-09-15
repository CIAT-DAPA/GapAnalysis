#'
#' @title Final conservation score in situ
#' @name FCSin
#' @description This function calculates the average of the three in situ conservation metrics and
#' assigns a priority category based on the results
#'
#' @param taxon A character object that defines the name of the species as listed in the occurrence dataset
#' @param srsin A dataframe contain the results from the srsin function
#' @param grsin A dataframe contain the results from the grsin function
#' @param ersin A dataframe contain the results from the ersin function
#'
#' @return out_df : a data frames of values summarizing the results of the function
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
#' # generate insitu conservation summaries
#' srs_insitu <- SRSin(taxon = taxon,
#'                     sdm = sdm,
#'                     occurrenceData = occurrenceData,
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
#' FCSin <- FCSin(taxon = taxon,
#'                     srsin = srs_insitu,
#'                     grsin = grs_insitu,
#'                     ersin = ers_insitu
#'                     )
#'
#'
#'
#' @references
#' Khoury et al. (2019) Ecological Indicators 98:420-429. doi: 10.1016/j.ecolind.2018.11.016
#' Carver et al. (2021) GapAnalysis: an R package to calculate conservation indicators using spatial information
#' @importFrom dplyr tibble
#' @export

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
  out_df$"FCS insitu score" <- score
  return(out_df)
}
