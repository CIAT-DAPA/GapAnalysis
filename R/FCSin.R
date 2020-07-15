#' @title Final conservation score in situ
#' @name FCSin
#' @description This function calculates the average of the three in situ conservation metrics and
#' assigns a priority category based on the results
#' @param Occurrence_data A data frame object with the species name, geographical coordinates,
#'  and type of records (G or H) for a given species
#' @param Species_list A vector of characters with the species names to calculate the GRSex metrics.
#' @param Raster_list A list of rasters representing the species distribution models for the species list provided
#'  in \var{Species_list}. The order of rasters in this list must match the same order as \var{Species_list}.
#' @param Ecoregions_shp A shapefile representing Ecoregions_shp information with a field ECO_NUM
#'  representing Ecoregions_shp Ids.If Ecoregions_shp=NULL the funtion will use
#'  an ecoregions raster file provided for your use
#' @param Pro_areas A raster file representing protected areas information.If Pro_areas=NULL the funtion will use
#'  a protected area raster file provided for your use
#'  after run GetDatasets()
#' @param Gap_Map logical, if \code{TRUE} the function will calculate gap maps for each species analyzed and will return a list
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
#' Cucurbita_splist <- unique(CucurbitaData$species)
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
#'                                       Gap_Map=FALSE)
#'
#' @references
#'
#' Khoury et al. (2019) Diversity and Distributions 26(2):209-225. doi: 10.1111/DDI.13008
#'
#' @export

#' @importFrom raster overlay crop raster extent

FCSin <- function(Species_list, Occurrence_data, Raster_list,Ecoregions_shp=NULL,Pro_areas=NULL,Gap_Map=FALSE) {
  SRSin_df <- NULL
  GRSin_df <- NULL
  ERSin_df <- NULL
  FCSIn_df <- NULL


  #Checking Occurrence_data format
  par_names <- c("species","latitude","longitude","type")


  if(missing(Occurrence_data)){
    stop("Please add a valid data frame with columns: species, latitude, longitude, type")
  }

  if(isFALSE(identical(names(Occurrence_data),par_names))){
    stop("Please format the column names in your dataframe as species, latitude, longitude, type")
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

  #Checking if Gap_Map option is a boolean or if the parameter is missing left Gap_Map as FALSE
  if(is.null(Gap_Map) | missing(Gap_Map)){ Gap_Map <- FALSE
  } else if(isTRUE(Gap_Map) | isFALSE(Gap_Map)){
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


  if(isFALSE(Gap_Map) | is.null(Gap_Map)){
    # join the dataframes based on species
  FCSin_df <- merge(SRSin_df, GRSin_df, by ="species",all.x=TRUE)
  FCSin_df <- merge(FCSin_df, ERSin_df, by = "species",all.x=TRUE)
      #dplyr::select("species","SRSin", "GRSin", "ERSin")
  } else {
    FCSin_df <- merge(SRSin_df$SRSin, GRSin_df$GRSin, by ="species",all.x=TRUE)
    FCSin_df <- merge(FCSin_df, ERSin_df$ERSin, by = "species",all.x=TRUE)

  }
    # calculate the mean value for each row to determine fcs per species
  FCSin_df$FCSin <- rowMeans(FCSin_df[, c("SRSin", "GRSin", "GRSin")])



   #assign classes (exsitu)

  FCSin_df$FCSin_class <- with(FCSin_df, ifelse(FCSin < 25, "HP",
                                                ifelse(FCSin >= 25 & FCSin < 50, "MP",
                                                       ifelse(FCSin >= 50 & FCSin < 75, "LP",
                                                              "SC"))))



  if(isTRUE(Gap_Map)){
    FCSin_df <- list(FCSin=FCSin_df,SRSin_maps=SRSin_df$gap_maps,GRSin_maps=GRSin_df$GapMapIn_list,ERSin_maps=ERSin_df$gap_maps)
  } else{
    FCSin_df <- FCSin_df
  }
  return(FCSin_df)
  }
