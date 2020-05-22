#' @title Combining ex situ and in situ gap analyses
#' @name FCSc_mean
#' @description This function creates a final combined conservation score by averaging the FCSex and FCSin
#'  It also assigns priority categories based on quantitative results
#' @param FCSex_df A data frame or a list object result of the function FCSex
#' @param FCSin_df A data frameor a list  object result of the function FCSin
#'
#' @return this function returns a data frame object with the following columns:
#'
#' \tabular{lcc}{
#'  species \tab Species name \cr
#'  FCSex \tab Final conservation score ex situ  \cr
#'  FCSin \tab Final conservation score in situ  \cr
#'  FCS_min \tab Final conservation score (mininum value among FCSin and FCSex) \cr
#'  FCS_max \tab Final conservation score (maximum value among FCSin and FCSex) \cr
#'  FCSc_mean \tab Final conservation score combined (average value between FCSin and FCSex) \cr
#'  FCS_min_class \tab Final conservation category using FCS_min value \cr
#'  FCS_max_class \tab Final conservation category using FCS_max value \cr
#'  FCSc_mean_class \tab Final conservation category using FCSc_mean value \cr
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
#' #Running all three ex situ gap analysis steps using FCSex function
#' FCSex_df <- FCSex(Species_list=Cucurbita_splist,
#'                                       Occurrence_data=CucurbitaData,
#'                                       Raster_list=CucurbitaRasters,
#'                                       Buffer_distance=50000,
#'                                       Ecoregions_shp=ecoregions
#'                                       )
#'
#'
#' #Running all three in situ gap analysis steps using FCSin function
#' FCSin_df <- FCSin(Species_list=Cucurbita_splist,
#'                                       Occurrence_data=CucurbitaData,
#'                                       Raster_list=CucurbitaRasters,
#'                                       Ecoregions_shp=ecoregions,
#'                                       Pro_areas=ProtectedAreas)
#'
#' FCSc_mean_df <- FCSc_mean(FCSex_df = FCSex_df,FCSin_df = FCSin_df)
#'
#' @references
#'
#' Khoury et al. (2019) Diversity and Distributions 26(2): 209-225. doi: 10.1111/DDI.13008
#'
#' @export
#' @importFrom magrittr %>%
#' @importFrom dplyr left_join select


FCSc_mean <- function(FCSex_df, FCSin_df) {
  df <- NULL

  if(class(FCSex_df)=="list"){
    FCSex_df <- FCSex_df$FCSex
  } else {
    FCSex_df <- FCSex_df
  }

  if(class(FCSin_df )=="list"){
    FCSin_df  <- FCSin_df$FCSin
  } else {
    FCSin_df  <- FCSin_df
  }
  #join datasets and select necessary Columns
  df <- dplyr::left_join(x = FCSex_df, y = FCSin_df, by = "species")
  df <-  df[,c("species", "FCSex","FCSex_class", "FCSin", "FCSin_class")]


  for(i in seq_len(nrow(df))){
    #compute FCS_min and FCS_min
    df$FCS_min[i] <- min(c(df$FCSex[i],df$FCSin[i]),na.rm=TRUE)
    df$FCS_max[i] <- max(c(df$FCSex[i],df$FCSin[i]),na.rm=TRUE)
    df$FCSc_mean[i] <- mean(c(df$FCSex[i],df$FCSin[i]),na.rm=TRUE)

    #assign classes (min)
    if (df$FCS_min[i] < 25) {
      df$FCS_min_class[i] <- "HP"
    } else if (df$FCS_min[i] >= 25 & df$FCS_min[i] < 50) {
      df$FCS_min_class[i] <- "MP"
    } else if (df$FCS_min[i] >= 50 & df$FCS_min[i] < 75) {
      df$FCS_min_class[i] <- "LP"
    } else {
      df$FCS_min_class[i] <- "SC"
    }

    #assign classes (max)
    if (df$FCS_max[i] < 25) {
      df$FCS_max_class[i] <- "HP"
    } else if (df$FCS_max[i] >= 25 & df$FCS_max[i] < 50) {
      df$FCS_max_class[i] <- "MP"
    } else if (df$FCS_max[i] >= 50 & df$FCS_max[i] < 75) {
      df$FCS_max_class[i] <- "LP"
    } else {
      df$FCS_max_class[i] <- "SC"
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
