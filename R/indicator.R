#' @title Conservation indicator across taxa based on gap analysis results
#' @name indicator
#' @description This function uses a data.frame resulting from the function FCSc_mean and
#'  computes a conservation indicator across taxa.
#' @param FCSc_mean_df A data frame object result of the function FCSc_mean
#' @return this function returns a data frame object with the following columns:
#'
#' \tabular{lcc}{
#'  opt \tab Final conservation score option \cr
#'  count_HP \tab Count of taxa high priority for conservation action \cr
#'  count_MP \tab Count of taxa medium priority for conservation action \cr
#'  count_LP \tab Count of taxa low priority for conservation action \cr
#'  count_SC \tab Count of taxa sufficiently conserved \cr
#'  count_LP_SC Count of taxa low priority for conservation action or sufficiently conserved  \tab \cr
#'  proportion_HP Proportion of taxa high priority for conservation action\tab \cr
#'  proportion_MP \tab Proportion of taxa medium priority for conservation action   \cr
#'  proportion_LP \tab Proportion of taxa low priority for conservation action \cr
#'  proportion_SC \  Proportion of taxa sufficiently conserved  \cr
#'  proportion_LP_SC \tab Proportion of taxa low priority for conservation action or sufficiently conserved (indicator) \cr
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
#'                   Occurrence_data=CucurbitaData,
#'                   Raster_list=CucurbitaRasters,
#'                   Buffer_distance=50000,
#'                   Ecoregions_shp=ecoregions)
#'
#'
#' #Running all three in situ gap analysis steps using FCSin function
#' FCSin_df <- FCSin(Species_list=Cucurbita_splist,
#'                   Occurrence_data=CucurbitaData,
#'                   Raster_list=CucurbitaRasters,
#'                   Ecoregions_shp=ecoregions,
#'                   Pro_areas=ProtectedAreas)
#'
#' FCSc_mean_df <- FCSc_mean(FCSex_df = FCSex_df,FCSin_df = FCSin_df)
#'
#'
#' indicator_df  <- indicator(FCSc_mean_df)
#'
#'@references
#'
#' Khoury et al. (2019) Ecological Indicators 98:420-429. doi: 10.1016/j.ecolind.2018.11.016
#'
#' @export


indicator <- function(FCSc_mean_df) {

  opt <- c("min","max","mean","in","ex")
  data_all <- FCSc_mean_df

  #make final counts for species list (combined)
  out_df <- data.frame()
  for (i in seq_len(length(opt))){
    #cat(i,"\n")
    #  i <- 5
    if(i==4 | i==5){    tvec <- paste(data_all[,paste("FCS",opt[i],sep="")])
    } else if(i==3){tvec <- paste(data_all[,paste("FCSc_",opt[i],"_class",sep="")])
    } else {
      tvec <- paste(data_all[,paste("FCS_",opt[i],"_class",sep="")])}
    hp_n <- length(which(tvec %in% c("HP")))
    mp_n <- length(which(tvec %in% c("MP")))
    lp_n <- length(which(tvec %in% c("LP")))
    sc_n <- length(which(tvec %in% c("SC")))
    indic <- lp_n + sc_n
    tdf <- data.frame(opt=opt[i],count_HP=hp_n,count_MP=mp_n,count_LP=lp_n,count_SC=sc_n,count_LP_SC=indic)
    out_df <- rbind(out_df, tdf)
  }

  #make final counts for species list (exsitu) if asked to
  if ("ex" %in% tolower(opt)) {
    tvec <- paste(data_all[,"FCSex_class"])
    hp_n <- length(which(tvec %in% c("HP")))
    mp_n <- length(which(tvec %in% c("MP")))
    lp_n <- length(which(tvec %in% c("LP")))
    sc_n <- length(which(tvec %in% c("SC")))
    indic <- lp_n + sc_n
    out_df_ex <- data.frame(opt="exsitu",count_HP=hp_n,count_MP=mp_n,count_LP=lp_n,count_SC=sc_n,count_LP_SC=indic)
    out_df <- rbind(out_df, out_df_ex)
    #out_df[5,2:6] <- out_df_ex[1,2:6]
  }



  #make final counts for species list (insitu)
  if ("in" %in% tolower(opt)) {
    tvec <- paste(data_all[,"FCSin_class"])
    hp_n <- length(which(tvec %in% c("HP")))
    mp_n <- length(which(tvec %in% c("MP")))
    lp_n <- length(which(tvec %in% c("LP")))
    sc_n <- length(which(tvec %in% c("SC")))
    indic <- lp_n + sc_n
    out_df_in <- data.frame(opt="insitu",count_HP=hp_n,count_MP=mp_n,count_LP=lp_n,count_SC=sc_n,count_LP_SC=indic)
    out_df <- rbind(out_df, out_df_in)
    #out_df[4,2:6] <- out_df_in[1,2:6]

  }

  #calculate percentages
  out_df$proportion_HP <- out_df$count_HP / nrow(data_all) * 100
  out_df$proportion_MP <- out_df$count_MP / nrow(data_all) * 100
  out_df$proportion_LP <- out_df$count_LP / nrow(data_all) * 100
  out_df$proportion_SC <- out_df$count_SC / nrow(data_all) * 100
  out_df$proportion_LP_SC <- out_df$count_LP_SC / nrow(data_all) * 100

  out_df<-out_df[-c(4,5), ]
  return(out_df)
}

