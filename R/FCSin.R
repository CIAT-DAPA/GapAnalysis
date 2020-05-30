#' @title Final conservation score in situ
#' @name FCSin
#' @description This function calculates the average of the three in situ conservation metrics and
#' assigns a priority category based on the results
#' @param Species_list A species list to calculate metrics.
#' @param Occurrence_data A data frame object with the species name, geographical coordinates,
#'  and type of records (G or H) for a given species
#' @param Raster_list A list representing the species distribution models for the species list provided
#'  loaded in raster format. This list must match the same order as the species list.
#' @param Ecoregions_shp A shapefile representing Ecoregions_shp information with a field ECO_NUM
#'  representing Ecoregions_shp Ids.If Ecoregions_shp=NULL the funtion will use
#'  an ecoregions raster file provided for your use
#' @param Pro_areas A raster file representing protected areas information.If Pro_areas=NULL the funtion will use
#'  a protected area raster file provided for your use
#'  after run GetDatasets()
#' @param Gap_Map Default=NULL, This option will calculate gap maps for each species analyzed and will return a list
#'  with four slots FCSin, SRSin_maps,GRSin_maps,and ERSin_maps
#' @return This function returns a data frame summarizing the in situ gap analysis scores:
#' \tabular{lcc}{
#' species \tab Species name \cr
#' SRSin \tab Sampling representativeness score in situ  \cr
#' GRSin \tab Geographical representativeness score in situ \cr
#' ERSin \tab Ecological representativeness score in situ \cr
#' FCSin \tab Final conservation score in situ  \cr
#' }
#' @examples
#' ##Obtaining occurrences from example
#' data(CucurbitaData)
#' ##Obtaining species names from the data
#' Cucurbita_splist <- unique(CucurbitaData$taxon)
#' ##Obtaining Raster_list
#' data(CucurbitaRasters)
#' CucurbitaRasters <- raster::unstack(CucurbitaRasters)
#' ##Obtaining protected areas raster
#' data(ProtectedAreas)
#' ##Obtaining Ecoregions_shpions shapefile
#' data(ecoregions)
#' #Running all three In-situ gap analysis steps using a unique function
#' FCSin_df <- FCSin(Species_list=Cucurbita_splist,
#'                                       Occurrence_data=CucurbitaData,
#'                                       Raster_list=CucurbitaRasters,
#'                                       Ecoregions_shp=ecoregions,
#'                                       Pro_areas=ProtectedAreas,
#'                                       Gap_Map=NULL)
#'
#' @references
#'
#' Khoury et al. (2019) Diversity and Distributions 26(2):209-225. doi: 10.1111/DDI.13008
#'
#' @export

#' @importFrom raster overlay crop raster extent

FCSin <- function(Species_list, Occurrence_data, Raster_list,Ecoregions_shp=NULL,Pro_areas=NULL,Gap_Map=NULL) {
  SRSin_df <- NULL
  GRSin_df <- NULL
  ERSin_df <- NULL
  FCSIn_df <- NULL


  #Checking Occurrence_data format
  par_names <- c("taxon","latitude","longitude","type")
  if(identical(names(Occurrence_data),par_names)==FALSE){
    stop("Please format the column names in your dataframe as taxon,latitude,longitude,type")
  }

  # load in protected area raster
  if(is.null(Pro_areas) | missing(Pro_areas)){
    if(file.exists(system.file("data/preloaded_data/protectedArea/wdpa_reclass.tif",
                               package = "GapAnalysis"))){
      Pro_areas <- raster::raster(system.file("data/preloaded_data/protectedArea/wdpa_reclass.tif",
                                              package = "GapAnalysis"))
    } else {
      stop("Protected areas file is not available yet. Please run the function GetDatasets()  and try again")
    }
  } else{
    Pro_areas <- Pro_areas
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

  #Checking if GapMapEx option is a boolean
  if(is.null(Gap_Map) | missing(Gap_Map)){ Gap_Map <- FALSE
  } else if(Gap_Map==TRUE | Gap_Map==FALSE){
    Gap_Map <- Gap_Map
  } else {
    stop("Choose a valid option for GapMap (TRUE or FALSE)")
  }


  # call SRSin
  SRSin_df <- SRSin(Species_list = Species_list,
                                 Occurrence_data = Occurrence_data,
                                 Raster_list = Raster_list,
                                 Pro_areas=Pro_areas,
                                 Gap_Map = Gap_Map)

  GRSin_df <- GRSin(Species_list = Species_list,
                                 Occurrence_data = Occurrence_data,
                                 Raster_list = Raster_list,
                                 Pro_areas=Pro_areas,
                                 Gap_Map = Gap_Map)

  ERSin_df <- ERSin(Species_list = Species_list,
                                 Occurrence_data =Occurrence_data,
                                 Raster_list = Raster_list,
                                 Pro_areas=Pro_areas,
                                 Ecoregions_shp=Ecoregions_shp,
                                 Gap_Map = Gap_Map)


  if(Gap_Map==FALSE | is.null(Gap_Map)){
    # join the dataframes based on species
  FCSin_df <- merge(SRSin_df, GRSin_df, by ="species")
  FCSin_df <- merge(FCSin_df, ERSin_df, by = "species")
      #dplyr::select("species","SRSin", "GRSin", "ERSin")
  } else {
    FCSin_df <- merge(SRSin_df$SRSin, GRSin_df$GRSin, by ="species")
    FCSin_df <- merge(FCSin_df, ERSin_df$ERSin, by = "species")

  }
    # calculate the mean value for each row to determine fcs per species
    for(i in seq_len(nrow(FCSin_df))){
      FCSin_df$FCSin[i] <- base::mean(c(FCSin_df$SRSin[i], FCSin_df$GRSin[i], FCSin_df$ERSin[i]))
    };rm(i)
  #assign classes (exsitu)
  FCSin_df$FCSin_class <- NA
  for (i in seq_len(nrow(FCSin_df))){
    if (FCSin_df$FCSin[i] < 25) {
      FCSin_df$FCSin_class[i] <- "HP"
    } else if (FCSin_df$FCSin[i] >= 25 & FCSin_df$FCSin[i] < 50) {
      FCSin_df$FCSin_class[i] <- "MP"
    } else if (FCSin_df$FCSin[i] >= 50 & FCSin_df$FCSin[i] < 75) {
      FCSin_df$FCSin_class[i] <- "LP"
    } else {
      FCSin_df$FCSin_class[i] <- "SC"
    }
  }

  if(Gap_Map==TRUE){
    FCSin_df <- list(FCSin=FCSin_df,SRSin_maps=SRSin_df$gap_maps,GRSin_maps=GRSin_df$GapMapIn_list,ERSin_maps=ERSin_df$gap_maps)
  } else{
    FCSin_df <- FCSin_df
  }
  return(FCSin_df)
  }
