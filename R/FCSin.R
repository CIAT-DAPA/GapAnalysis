#' @title Final conservation score in situ
#' @name FCSin
#' @description This function calculates the average of the three in situ conservation metrics
#'
#' @param Species_list A species list to calculate metrics.
#' @param Occurrence_data A data frame object with the species name, geographical coordinates,
#'   and type of records (G or H) for a given species
#' @param Raster_list A list representing the species distribution models for the species list provided
#'  loaded in raster format. This list must match the same order of the species list.
#' @param Ecoregions_shp A shapefile representing Ecoregions_shp information with a field ECO_NUM representing Ecoregions_shp Ids.
#'  If Ecoregions_shp=NULL the funtion will use a shapefile provided for your use after run GetDatasets()
#' @param Pro_areas A raster file representing protected areas information.
#'  If Pro_areas=NULL the funtion will use a protected area raster file provided for your use after run GetDatasets()
#' @param Gap_MapIn Default=FALSE, This option will calculate gap maps for each species analyzed and will return a list
#' with two slots FCSin and gap_maps
#'
#' @return This function returns a list with gap maps if Gap_MapIn=TRUE. Otherwise, it returns a data frame
#'  summarizing the in situ gap analysis scores:
#'
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
#'                                       Gap_MapIn=FALSE)
#'
#'@reference
#' Khoury et al. (2019) Diversity and Distributions 26(2):209-225. doi: 10.1111/DDI.13008
#'
#' @export
#' @importFrom dplyr left_join
#' @importFrom raster overlay crop raster extent

FCSin <- function(Species_list, Occurrence_data, Raster_list,Ecoregions_shp,Pro_areas,Gap_MapIn) {
  SRSin_df <- NULL
  GRSin_df <- NULL
  ERSin_df <- NULL
  FCSIn_df <- NULL


  #Checking Occurrence_data format
  par_names <- c("taxon","latitude","longitude","type")
  if(identical(names(Occurrence_data),par_names)==FALSE){
    stop("Please format the column names in your dataframe as taxon,latitude,longitude,type")
  }

  #Checking if GapMapIn option is a boolean

  if(is.null(Gap_MapIn)){ Gap_MapIn <- FALSE
  } else if(Gap_MapIn==TRUE | Gap_MapIn==FALSE){
    Gap_MapIn <- Gap_MapIn
  } else {
    stop("Choose a valid option for Gap_MapIn (TRUE or FALSE)")
  }


  # load in protected areas raster
  if(is.null(Pro_areas)){
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
  if(is.null(Ecoregions_shp)){
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

  # call SRSin
  SRSin_df <- SRSin(Species_list = Species_list,
                                 Occurrence_data = Occurrence_data,
                                 Raster_list = Raster_list,
                                 Pro_areas=Pro_areas)
  # call GRSin
  GRSin_df <- GRSin(Species_list = Species_list,
                                 Occurrence_data = Occurrence_data,
                                 Raster_list = Raster_list,
                                 Pro_areas=Pro_areas)
  # call ERSin
  ERSin_df <- ERSin(Species_list = Species_list,
                                 Occurrence_data =Occurrence_data,
                                 Raster_list = Raster_list,
                                 Pro_areas=Pro_areas,
                                 Ecoregions_shp=Ecoregions_shp)


    # join the dataframes base on species
  FCSin_df <- dplyr::left_join(SRSin_df, GRSin_df, by ="species")
  FCSin_df <- dplyr::left_join(FCSin_df, ERSin_df, by = "species")
      #dplyr::select("species","SRSin", "GRSin", "ERSin")
    # calculate the mean value for each row to determine fcs per species
    for(i in seq_len(nrow(FCSin_df))){
      FCSin_df$FCSin[i] <- base::mean(c(FCSin_df$SRSin[i], FCSin_df$GRSin[i], FCSin_df$ERSin[i]))
    };rm(i)

  #Gap_MapIn

  if(Gap_MapIn==T){
    Gap_MapIn_list <- list()

    cat("Calculating gap maps for in situ conservation gap analysis","\n")


    for(i in seq_len(length(Raster_list))){
      sdm_temp <-  Raster_list[[i]]
      if(raster::extent(sdm_temp)!=raster::extent(Pro_areas)){
        Pro_areas <- raster::crop(x = Pro_areas,y = sdm_temp)
      } else {
        Pro_areas <- Pro_areas
      }

      gap_map <- raster::overlay(Pro_areas,sdm_temp,fun=function(r1, r2){return(r1-r2)})
      gap_map[which(gap_map[]==0)] <- NA
      Gap_MapIn_list[[i]] <- gap_map
      names(Gap_MapIn_list[[i]] ) <- Species_list[[i]]
    };rm(i)

    FCSin_df <- list(FCSin=FCSin_df,Gap_MapIn_list=Gap_MapIn_list)
  } else {
    FCSin_df <- FCSin_df
  }
  return(FCSin_df)
  }
