#' @title Final in-situ conservation score estimation (In-situ conservation)
#' @name FCSin
#' @description TThis function allows calculate all the three In-situ gap analysis scores
#' in an unique dataframe object to calculate a final priority score as
#' the average of the SRSin, GRSin, and ERSin values:
#'
#' \deqn{FCSin = mean(SRSin,GRSin,ERSin)}
#'
#' @param Species_list A species list to calculate metrics.
#' @param Occurrence_data A data frame object with the species name, geographical coordinates,
#'   and type of records (G or H) for a given species
#' @param Raster_list A list representing the species distribution models for the species list provided
#'  loaded in raster format. This list must match the same order of the species list.
#' @param Ecoregions_shp A shapefile representing Ecoregions_shpions information with a field ECO_NUM representing Ecoregions_shpions Ids.
#'  If Ecoregions_shp=NULL the funtion will use a shapefile provided for your use after run GetDatasets()
#' @param Pro_areas A raster file representing protected areas information.
#'  If Pro_areas=NULL the funtion will use a protected area raster file provided for your use after run GetDatasets()
#'
#' @return This function returns a list with gap maps if Gap_MapIn=TRUE. Otherwise, it returns a data frame
#'  summarizing the in-situ gap analysis scores:
#'
#' \tabular{lcc}{
#' species \tab Species name \cr
#' SRSin \tab In-situ sample representativeness score \cr
#' GRSin \tab In-situ germplasm representativeness score \cr
#' ERSin \tab In-situ environmental representativeness score \cr
#' FCSin \tab In-situ final conservation score \cr
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
#'                                       Pro_areas=ProtectedAreas)
#'
#'@references
#'
#' Ramirez-Villegas, J., Khoury, C., Jarvis, A., Debouck, D. G., & Guarino, L. (2010).
#' A Gap Analysis Methodology for Collecting Crop Genepools: A Case Study with Phaseolus Beans.
#' PLOS ONE, 5(10), e13497. Retrieved from https://doi.org/10.1371/journal.pone.0013497
#'
#' Khoury, C. K., Amariles, D., Soto, J. S., Diaz, M. V., Sotelo, S., Sosa, C. C., … Jarvis, A. (2019).
#' Comprehensiveness of conservation of useful wild plants: An operational indicator for biodiversity
#' and sustainable development targets. Ecological Indicators. https://doi.org/10.1016/j.ecolind.2018.11.016
#'
#' @export
#' @importFrom dplyr left_join
#' @importFrom raster overlay crop raster extent

FCSin <- function(Species_list, Occurrence_data, Raster_list,Ecoregions_shp=NULL,Pro_areas=NULL) {
  SRSin_df <- NULL
  GRSin_df <- NULL
  ERSin_df <- NULL
  FCSIn_df <- NULL


  #Checking Occurrence_data format
  par_names <- c("taxon","latitude","longitude","type")
  if(identical(names(Occurrence_data),par_names)==FALSE){
    stop("Please format the column names in your dataframe as taxon,latitude,longitude,type")
  }

  # load in protect area raster
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
                                 Ecoregions_shp=Ecoregions_shp,
                                 Gap_Map=FALSE)


    # join the dataframes base on species
  FCSin_df <- dplyr::left_join(SRSin_df, GRSin_df, by ="species")
  FCSin_df <- dplyr::left_join(FCSin_df, ERSin_df$ERSin, by = "species")
      #dplyr::select("species","SRSin", "GRSin", "ERSin")
    # calculate the mean value for each row to determine fcs per species
    for(i in seq_len(nrow(FCSin_df))){
      FCSin_df$FCSin[i] <- base::mean(c(FCSin_df$SRSin[i], FCSin_df$GRSin[i], FCSin_df$ERSin[i]))
    };rm(i)
  #assign classes (exsitu)
  FCSin_df$FCSin_class <- NA
  for (i in 1:nrow(FCSin_df)) {
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


  return(FCSin_df)
  }
