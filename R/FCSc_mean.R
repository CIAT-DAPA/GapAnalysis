#' @title Combining Ex-situ and In-situ gap analysis results in one comprehensive conservation score (Summary Assessments)
#' @name FCSc_mean
#' @description This function concatenates ex-situ conservation scores (SRSex, GRSex, ERSex,FCSex), and  in situ scores (SRSin, GRSin, ERSin,FCSin)
#' in one unique table and calculate the final conservation score for a species using Khoury et al., (2019) methodology.
#'
#' @param FCSex_df A data frame or a list object result of the function FCSex
#' @param FCSin_df A data frameor a list  object result of the function FCSin
#'
#' @return this function returns a data frame object with the following columns:
#'
#' \tabular{lcc}{
#'  species \tab Species name \cr
#'  FCSex \tab Ex-situ final conservation score \cr
#'  FCSin \tab In-situ final conservation score \cr
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
#' data(CucurbitaData)
#' ##Obtaining species names from the data
#' Cucurbita_splist <- unique(CucurbitaData$taxon)
#' ##Obtaining raster_list
#' data(CucurbitaRasters)
#' CucurbitaRasters <- raster::unstack(CucurbitaRasters)
#' ##Obtaining protected areas raster
#' data(ProtectedAreas)
#' ##Obtaining ecoregions shapefile
#' data(ecoregions)
#'
#' #Running all three Ex-situ gap analysis steps using FCSex function
#' FCSex_df <- FCSex(Species_list=Cucurbita_splist,
#'                                       Occurrence_data=CucurbitaData,
#'                                       Raster_list=CucurbitaRasters,
#'                                       Buffer_distance=50000,
#'                                       Ecoregions_shp=ecoregions,
#'                                       Gap_MapEx=FALSE)
#'
#'
#' #Running all three In-situ gap analysis steps using FCSin function
#' FCSin_df <- FCSin(Species_list=Cucurbita_splist,
#'                                       Occurrence_data=CucurbitaData,
#'                                       Raster_list=CucurbitaRasters,
#'                                       Ecoregions_shp=ecoregions,
#'                                       Gap_MapIn=FALSE)
#'
#' fcsCombine <- FCSc_mean(FCSex_df = FCSex_df,FCSin_df = FCSin_df)
#'
#'@references
#' Ramirez-Villegas, J., Khoury, C., Jarvis, A., Debouck, D. G., & Guarino, L. (2010).
#' A Gap Analysis Methodology for Collecting Crop Genepools: A Case Study with Phaseolus Beans.
#' PLOS ONE, 5(10), e13497. Retrieved from https://doi.org/10.1371/journal.pone.0013497
#'
#' Khoury, C. K., Amariles, D., Soto, J. S., Diaz, M. V., Sotelo, S., Sosa, C. C., … Jarvis, A. (2019).
#' Comprehensiveness of conservation of useful wild plants: An operational indicator for biodiversity
#' and sustainable development targets. Ecological Indicators. https://doi.org/10.1016/j.ecolind.2018.11.016
#'
#' @export
#' @importFrom magrittr %>%
#' @importFrom dplyr left_join select


FCSc_mean <- function(FCSex_df, FCSin_df) {
  df <- NULL

  if(class(FCSex_df)==list){
    FCSex_df <- FCSex_df$FCSex
  } else {
    FCSex_df <- FCSex_df
  }

  if(class(FCSin_df )==list){
    FCSin_df  <- FCSin_df$FCSin
  } else {
    FCSin_df  <- FCSin_df
  }
  #join datasets and select necessary Columns
  df <- dplyr::left_join(x = FCSex_df, y = FCSin_df, by = "species")
  df <-  df[,c("species", "FCSex", "FCSin")]


  for(i in seq_len(nrow(df))){
    #compute FCSc_min and FCSc_max
    df$FCSc_min[i] <- min(c(df$FCSex[i],df$FCSin[i]),na.rm=TRUE)
    df$FCSc_max[i] <- max(c(df$FCSex[i],df$FCSin[i]),na.rm=TRUE)
    df$FCSc_mean[i] <- mean(c(df$FCSex[i],df$FCSin[i]),na.rm=TRUE)

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
