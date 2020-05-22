#' @title Final conservation score ex situ
#' @name FCSex
#' @description This function calculates the average of the three ex situ conservation metrics
#'   returning a final conservation score summary table.
#' @param Species_list A species list to calculate metrics.
#' @param Occurrence_data A data frame object with the species name, geographical coordinates,
#'   and type of records (G or H) for a given species
#' @param Raster_list A list representing the species distribution models for the species list provided
#'  loaded in raster format. This list must match the same order of the species list.
#' @param Buffer_distance Geographical distance used to create circular buffers around germplasm.
#'  Default: 50000 (50 km) around germplasm accessions (CA50)
#' @param Ecoregions_shp A shapefile representing ecoregions information with a field ECO_NUM representing ecoregions Ids.
#'  If ecoReg=NULL the funtion will use a shapefile provided for your use after run GetDatasets()
#' @return This function returns a list with gap maps if Gap_MapEx=TRUE. Otherwise, it returns a data frame
#' summarizing the ex-situ gap analysis scores:
#'
#' \tabular{lcc}{
#' species \tab Species name \cr
#' SRSex \tab Sampling representativeness score ex situ \cr
#' GRSex \tab Geographical representativeness score ex situ \cr
#' ERSex \tab Ecological representativeness score ex situ \cr
#' FCSex \tab Final conservation score ex situ  \cr
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
#' ##Obtaining ecoregions shapefile
#' data(ecoregions)
#'
#' #Running all three Ex-situ gap analysis steps using a unique function
#' FCSex_df <- FCSex(Species_list=Cucurbita_splist,
#'                                       Occurrence_data=CucurbitaData,
#'                                       Raster_list=CucurbitaRasters,
#'                                       Buffer_distance=50000,
#'                                       Ecoregions_shp=ecoregions)
#'
#'@references
#'
#' Khoury et al. (2019) Ecological Indicators 98:420-429. doi: 10.1016/j.ecolind.2018.11.016
#'
#' @export
#' @importFrom dplyr left_join


FCSex <- function(Species_list, Occurrence_data, Raster_list, Buffer_distance=50000,Ecoregions_shp=NULL){

  SRSex_df <- NULL
  GRSex_df <- NULL
  ERSex_df <- NULL
  FCSex_df <- NULL

  #Checking Occurrence_data format
  par_names <- c("taxon","latitude","longitude","type")
  if(identical(names(Occurrence_data),par_names)==FALSE){
    stop("Please format the column names in your dataframe as taxon,latitude,longitude,type")
  }

  # Load in ecoregions shp
  if(is.null(Ecoregions_shp) | missing(Ecoregions_shp)){
    if(file.exists(system.file("data/preloaded_data/ecoRegion/tnc_terr_ecoregions.shp",
                               package = "GapAnalysis"))){
      Ecoregions_shp <- raster::shapefile(system.file("data/preloaded_data/ecoRegion/tnc_terr_ecoregions.shp",
                                                      package = "GapAnalysis"),encoding = "UTF-8")
    } else {
      stop("Ecoregions file is not available yet. Please run the function GetDatasets() and try again")
    }
  } else{
    Ecoregions_shp <- Ecoregions_shp
  }


  # call SRSex
  SRSex_df <- SRSex(Species_list = Species_list,
                     Occurrence_data = Occurrence_data)
  # call GRSex
  GRSex_df <- GRSex(Occurrence_data = Occurrence_data,
                     Species_list = Species_list,
                    Raster_list = Raster_list,
                    Buffer_distance = Buffer_distance,
                    Gap_Map=NULL)
  # call ERSex
  ERSex_df <- ERSex(Species_list = Species_list,
                     Occurrence_data = Occurrence_data,
                    Raster_list = Raster_list,
                    Buffer_distance = Buffer_distance,
                    Ecoregions_shp=Ecoregions_shp,
                    Gap_Map=NULL)

  # join the dataframes base on species

  if(class(GRSex_df)!="list"){
    FCSex_df <- dplyr::left_join(SRSex_df, GRSex_df, by ="species")
  } else {
    FCSex_df <- dplyr::left_join(SRSex_df, GRSex_df$GRSex, by ="species")
  }


  FCSex_df <- dplyr::left_join(FCSex_df, ERSex_df$ERSex, by = "species")
  # calculate the mean value for each row to determine fcs per species
  for(i in seq_len(nrow(FCSex_df))){
    FCSex_df$FCSex[i] <- base::mean(c(FCSex_df$SRSex[i], FCSex_df$GRSex[i], FCSex_df$ERSex[i]))
  };rm(i)
  #assign classes (exsitu)
  FCSex_df$FCSex_class <- NA
  for (i in 1:nrow(FCSex_df)) {
    if (FCSex_df$FCSex[i] < 25) {
      FCSex_df$FCSex_class[i] <- "HP"
    } else if (FCSex_df$FCSex[i] >= 25 & FCSex_df$FCSex[i] < 50) {
      FCSex_df$FCSex_class[i] <- "MP"
    } else if (FCSex_df$FCSex[i] >= 50 & FCSex_df$FCSex[i] < 75) {
      FCSex_df$FCSex_class[i] <- "LP"
    } else {
      FCSex_df$FCSex_class[i] <- "SC"
    }
  }

  return(FCSex_df)
}
