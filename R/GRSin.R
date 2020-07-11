#' @title Geographical representativeness score in situ
#' @name GRSin
#' @description The GRSin process provides a geographic measurement of the proportion of a species’ range that can be considered
#' to be conserved in protected areas. The GRSin compares the area of the distribution model located within protected areas versus
#' the total area of the model, considering comprehensive conservation to have been accomplished only when the entire distribution
#' occurs within protected areas.
#'
#' @param Species_list A species list to calculate the GRSin metrics.
#' @param Occurrence_data A data frame object with the species name, geographical coordinates, and type of records (G or H) for a given species
#' @param Raster_list A list representing the species distribution models for the species list provided loaded in raster format.
#'  This list must match the same order as the species list.
#' @param Raster_list A list representing the species distribution models for the species list provided loaded in raster format. This list must match the same order of the species list.
#' @param Pro_areas A raster file representing protected areas information. If Pro_areas=NULL the function will use a protected area raster file
#'  provided for your use after run GetDatasets()
#' @param Gap_Map Default=NULL, This option will calculate gap maps for each species analyzed and will return a list
#'  with two slots ERSin and gap_maps
#'
#' @return This function returns a data frame with two columns:
#'
#' \tabular{lcc}{
#' species \tab Species name \cr
#' GRSin \tab GRSin value calculated\cr
#' }
#'
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
#' #Running GRSin
#' GRSin_df <- GRSin(Species_list = Cucurbita_splist,
#'                     Occurrence_data = CucurbitaData,
#'                     Raster_list = CucurbitaRasters,
#'                     Pro_areas=ProtectedAreas,
#'                     Gap_Map=NULL)
#'
#'
#'@references
#'
#' Khoury et al. (2019) Ecological Indicators 98:420-429. doi: 10.1016/j.ecolind.2018.11.016
#'
#' @export
#' @importFrom stats median
#' @importFrom raster raster crop area




GRSin <- function(Species_list,Occurrence_data,Raster_list,Pro_areas=NULL, Gap_Map=NULL){

# suppressMessages(require(rgdal))
# suppressMessages(require(raster))
# suppressMessages(require(tmap))
# suppressMessages(require(fasterize))
# suppressMessages(require(sf))

#importFrom("methods", "as")
#importFrom("stats", "complete.cases", "filter", "median")
#importFrom("utils", "data", "memory.limit", "read.csv", "write.csv")

  #Checking Occurrence_data format
  par_names <- c("taxon","latitude","longitude","type")


  if(missing(Occurrence_data)){
    stop("Please add a valid data frame with columns: taxon,latitude,longitude,type")
  }


  if(identical(names(Occurrence_data),par_names)==FALSE){
    stop("Please format the column names in your dataframe as taxon,latitude,longitude,type")
  }
  #Checking if user is using a raster list or a raster stack
  if(class(Raster_list)=="RasterStack"){
    Raster_list <- raster::unstack(Raster_list)
  } else {
    Raster_list <- Raster_list
  }

  #Checking if GapMapEx option is a boolean
  if(is.null(Gap_Map) | missing(Gap_Map)){ Gap_Map <- FALSE
  } else if(Gap_Map==TRUE | Gap_Map==FALSE){
    Gap_Map <- Gap_Map
  } else {
    stop("Choose a valid option for GapMap (TRUE or FALSE)")
  }

  df <- data.frame(matrix(ncol=2, nrow = length(Species_list)))
  colnames(df) <- c("species", "GRSin")
  # load in protected areas raster
  if(is.null(Pro_areas) | missing(Pro_areas)){
    if(file.exists(system.file("data/preloaded_data/protectedArea/wdpa_reclass.tif",package = "GapAnalysis"))){
      Pro_areas <- raster::raster(system.file("data/preloaded_data/protectedArea/wdpa_reclass.tif",package = "GapAnalysis"))
    } else {
      stop("Protected areas file is not available yet. Please run the function GetDatasets()  and try again")
    }
  } else{
    Pro_areas <- Pro_areas
  }

  if(Gap_Map==TRUE){
    GapMapIn_list <- list()
  }


  # loop over species list
  for(i in seq_len(length(Species_list))){
    # select threshold map for a given species
    for(j in seq_len(length(Raster_list))){
      if(grepl(j, i, ignore.case = TRUE)){
        sdm <- Raster_list[[j]]
      }
      d1 <- Occurrence_data[Occurrence_data$taxon == Species_list[i],]
      test <- GapAnalysis::paramTest(d1, sdm)
      if(test[1] == TRUE){
         stop(paste0("No Occurrence data exists, but and SDM was provide. Please check your occurrence data input for ", Species_list[i]))
    }

    };rm(j)

    if(test[2] == FALSE){
      df$species[i] <- as.character(Species_list[i])
      df$GRSin[i] <- 0
      paste0("Either no occurrence data or SDM was found for species ", as.character(Species_list[i]),
    " the conservation metric was automatically assigned 0")
    }else{
    # determine the area of predicted presence of a species based on the species distribution map
    sdm1 <- sdm
    Pro_areas1 <- raster::crop(x = Pro_areas,y = sdm1)
    sdm1[sdm1[] == 0] <- NA
    if(raster::res(Pro_areas1)[1] != raster::res(sdm)[1]){
      Pro_areas1 <- raster::resample(x = Pro_areas1, y = sdm)
    }
    cell_size <- raster::area(sdm1, na.rm=TRUE, weights=FALSE)
    cell_size <- cell_size[!is.na(cell_size)]
    thrshold_area <- length(cell_size)*median(cell_size)

    # mask the protected area raster to the species distribution map and calculate area
    Pro_areas1[Pro_areas1[] == 0] <-NA
    Pro_areas1 <- Pro_areas1 * sdm1
    # calculate area
    cell_size <- raster::area(Pro_areas1, na.rm=TRUE, weights=FALSE)
    cell_size <- cell_size[!is.na(cell_size)]
    protected_area <- length(cell_size)*stats::median(cell_size)
    if(!is.na(protected_area)){
      # calculate GRSin
      GRSin <- min(c(100, protected_area/thrshold_area*100))
      df$species[i] <- as.character(Species_list[i])
      df$GRSin[i] <- GRSin
    }else{
      df$species[i] <- as.character(Species_list[i])
      df$GRSin[i] <- 0
    }
    #GRSin gap map

    if(Gap_Map==TRUE){
      message(paste0("Calculating GRSin gap map for ",as.character(Species_list[i])),"\n")
      Pro_areas1[is.na(Pro_areas1),] <-  0
      gap_map <- sdm - Pro_areas1
      gap_map[gap_map == 0,] <- NA
      GapMapIn_list[[i]] <- gap_map
      names(GapMapIn_list[[i]] ) <- Species_list[[i]]
    }
  }
  if(Gap_Map==TRUE){
    df <- list(GRSin= df,gap_maps=GapMapIn_list)
  } else {
    df <- df
  }
}
  return(df)
}
