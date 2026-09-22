#' @title Final conservation score ex situ
#' @name FCSex
#' @description This function calculates the average of the three ex situ conservation metrics
#'   returning a final conservation score summary table. It also assigns conservation priority categories
#'
#' @param taxon A character object that defines the name of the species as listed in the occurrence dataset
#' @param srsex A dataframe contain the results from the srsex function
#' @param grsex A dataframe contain the results from the grsex function
#' @param ersex A dataframe contain the results from the ersex function
#' @param noModel A boolean parameter (TRUE/FALSE) to determine if there is a species distribution model. Default is FALSE.
#'   When TRUE, GRS and ERS cannot be calculated and are assigned 0; the final score is the mean of the three metrics,
#'   following Khoury et al. (2019).
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
#' ecoregions <- terra::unwrap(ecoregions)
#' # generate exsitu conservation summaries
#' srs_exsitu <- SRSex(taxon = taxon,
#'                     occurrenceData = CucurbitaData
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
#'                     occurrenceData = occurrenceData,
#'                     gBuffer = gBuffer,
#'                     ecoregions = ecoregions,
#'                     idColumn = "ECO_NAME"
#'                     )
#'
#' #Running fcsex
#' fcs_exsitu <- FCSex(taxon = taxon,
#'                     srsex = srs_exsitu,
#'                     grsex = grs_exsitu,
#'                     ersex = ers_exsitu)
#'
#'
#'
#' @references
#' Khoury et al. (2019) Ecological Indicators 98:420-429. \doi{10.1016/j.ecolind.2018.11.016}
#' Carver et al. (2021) GapAnalysis: an R package to calculate conservation indicators using spatial information
#' @export
#'

FCSex <- function(taxon, srsex, grsex, ersex, noModel = FALSE){

  # define variables
  srs <- srsex$`SRS exsitu`

  if (isTRUE(noModel)) {
    # no distribution model: GRS and ERS cannot be calculated and are set to 0
    # so the final score remains the mean of the three metrics (Khoury et al. 2019)
    grs <- 0
    ers <- 0
  } else {
    grs <- grsex$results$`GRS exsitu`
    ers <- ersex$results$`ERS exsitu`
  }

  # mean across the three measures
  sp_fcs <- mean(c(srs, grs, ers), na.rm = TRUE)

  out_df <- dplyr::tibble(Taxon = taxon,
                          "SRS exsitu" = srs,
                          "GRS exsitu" = grs,
                          "ERS exsitu" = ers,
                          "FCS exsitu" = sp_fcs,
                          "FCS exsitu score" = NA)

  sp_fcs <- out_df$"FCS exsitu"

  # assign classes
  if (sp_fcs < 25) {
    score <- "UP"
  } else if (sp_fcs >= 25 & sp_fcs < 50) {
    score <- "HP"
  } else if (sp_fcs >= 50 & sp_fcs < 75) {
    score <- "MP"
  } else {
    score <- "LP"
  }

  out_df$"FCS exsitu score" <- score
  return(out_df)
}
