#' @title Combining Ex situ and In situ gap analysis results in one comprehensive conservation score (Summary Assessments)
#' @name fcs_Combine
#' @description This function concatenates ex situ conservation scores (SRSex, GRSex, ERSex,FCSex), and  in situ scores (SRSin, GRSin, ERSin,FCSin)
#' in one unique table and calculate the final conservation score for a species using Khoury et al., (2019) methodology.
#'
#' @param fcsEx A data frame object result of the functions exsituGapAnalysis or fcs_exsitu
#' @param fcsIn A data frame object result of the functions insituGapAnalysis or fcs_insitu
#'
#' @return this function returns a data frame object with the following columns:
#'
#' \tabular{lcc}{
#'  species \tab Species name \cr
#'  FCSex \tab Ex situ final conservation score \cr
#'  FCSin \tab In situ final conservation score \cr
#'  FCSc_min \tab Final conservation score (mininum value between FCSin and FCSex) \cr
#'  FCSc_max \tab Final conservation score (maximum value between FCSin and FCSex) \cr
#'  FCSc_mean \tab Final conservation score (average value between FCSin and FCSex) \cr
#'  FCSc_min_class \tab Final conservation category using  FCSc_min value \cr
#'  FCSc_max_class \tab Final conservation category using  FCSc_max value \cr
#'  FCSc_mean_class \tab Final conservation category using  FCSc_mean value \cr
#' }
#'
#' @examples
#' ##Obtaining occurrences from example
#' data("cucurbitaData")
#' ##Obtaining species names from the data
#' speciesList <- unique(cucurbitaData$taxon)
#' ##Obtaining raster_list
#' data("sdm_rasters")
#' ##Obtaining protected areas raster
#' data("protectedArea")
#' ##Obtaining ecoregions shapefile
#' data("ecoregions")
#'
#' #Running all three Ex situ gap analysis steps using insituGapAnalysis function
#' exsituGapMetrics <- exsitu_GapAnalysis(species_list=speciesList,
#'                                       occurrenceData=cucurbitaData,
#'                                       raster_list=sdm_rasters,
#'                                       bufferDistance=50000,
#'                                       ecoReg=ecoregions)
#'
#'
#' #Running all three In situ gap analysis steps using insituGapAnalysis function
#' insituGapMetrics <- insitu_GapAnalysis(species_list=speciesList,
#'                                        occurrenceData=cucurbitaData,
#'                                        raster_list=sdm_rasters,
#'                                        proArea=protectedArea,
#'                                        ecoReg=ecoregions)
#'
#' fcsCombine <- fcs_Combine(fcsEx = exsituGapMetrics,fcsIn = insituGapMetrics)
#'
#'@references
#' Ramírez-Villegas, J., Khoury, C., Jarvis, A., Debouck, D. G., & Guarino, L. (2010).
#' A Gap Analysis Methodology for Collecting Crop Genepools: A Case Study with Phaseolus Beans.
#' PLOS ONE, 5(10), e13497. Retrieved from https://doi.org/10.1371/journal.pone.0013497
#'
#' Khoury, C. K., Amariles, D., Soto, J. S., Diaz, M. V., Sotelo, S., Sosa, C. C., … Jarvis, A. (2019).
#' Comprehensiveness of conservation of useful wild plants: An operational indicator for biodiversity
#' and sustainable development targets. Ecological Indicators. https://doi.org/10.1016/j.ecolind.2018.11.016
#'
#' @export
#' @importFrom magrittr %>%


fcs_Combine <- function(fcsEx, fcsIn) {

  #importFrom("methods", "as")
  #importFrom("stats", "complete.cases", "filter", "median")
  #importFrom("utils", "data", "memory.limit", "read.csv", "write.csv")

  #join datasets and select necessary Columns
  df <- dplyr::left_join(x = fcsEx, y = fcsIn, by = "species") %>%
    dplyr::select("species", "FCSex", "FCSin")


  for(i in 1:nrow(df)){
    #compute FCSc_min and FCSc_max
    df$FCSc_min[i] <- min(c(df$FCSex[i],df$FCSin[i]),na.rm=T)
    df$FCSc_max[i] <- max(c(df$FCSex[i],df$FCSin[i]),na.rm=T)
    df$FCSc_mean[i] <- mean(c(df$FCSex[i],df$FCSin[i]),na.rm=T)

    #assign classes (min)
    if (df$FCSc_min[i] < 25) {
      df$FCSc_min_class[i] <- "HP"
    } else if (df$FCSc_min[i] >= 25 & df$FCSc_min[i] < 50) {
      df$FCSc_min_class[i] <- "MP"
    } else if (df$FCSc_min[i] >= 50 & df$FCSc_min[i] < 75) {
      df$FCSc_min_class[i] <- "LP"
    } else {
      df$FCSc_min_class[i] <- "SC"
    }

    #assign classes (max)
    if (df$FCSc_max[i] < 25) {
      df$FCSc_max_class[i] <- "HP"
    } else if (df$FCSc_max[i] >= 25 & df$FCSc_max[i] < 50) {
      df$FCSc_max_class[i] <- "MP"
    } else if (df$FCSc_max[i] >= 50 & df$FCSc_max[i] < 75) {
      df$FCSc_max_class[i] <- "LP"
    } else {
      df$FCSc_max_class[i] <- "SC"
    }

    #assign classes (mean)
    if (df$FCSc_mean[i] < 25) {
      df$FCSc_mean_class[i] <- "HP"
    } else if (df$FCSc_mean[i] >= 25 & df$FCSc_mean[i] < 50) {
      df$FCSc_mean_class[i] <- "MP"
    } else if (df$FCSc_mean[i] >= 50 & df$FCSc_mean[i] < 75) {
      df$FCSc_mean_class[i] <- "LP"
    } else {
      df$FCSc_mean_class[i] <- "SC"
    }

  }
  return(df)
}
