#' @title Sampling representativeness score in situ
#' @name SRSin
#' @description The SRSin process calculates the proportion of all occurrences of a taxon falling within
#' the distribution model that also fall within a protected area
#' @param Species_list A species list to calculate the SRSin metric.
#' @param Occurrence_data A data frame object with the species name, geographical coordinates,
#'  and type of records (G or H) for a given species
#' @param Raster_list A list representing the species distribution models for the species list
#'  provided loaded in raster format. This list must match the same order as the species list.
#' @param Pro_areas A raster file representing protected areas information.
#'  If Pro_areas=NULL the function will use a protected area raster file
#'  provided for your use after run GetDatasets()
#' @return This function returns a data frame with two columns:
#'
#' \tabular{lcc}{
#' species \tab Species name \cr
#' SRSin \tab SRSin value calculated\cr
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
#' #Running SRSin
#' SRSin_df <- SRSin(Species_list = Cucurbita_splist,
#'                     Occurrence_data = CucurbitaData,
#'                     Raster_list=CucurbitaRasters,
#'                     Pro_areas=ProtectedAreas,
#'                     Gap_Map=NULL)
#'
#'@references
#'
#' Khoury et al. (2019) Diversity and Distributions 26(2):209-225. doi: 10.1111/DDI.13008.
#'
#' @export
#' @importFrom raster raster crop


SRSin <- function(Species_list, Occurrence_data, Raster_list,Pro_areas=NULL, Gap_Map=NULL){

  taxon <- NULL
  longitude <- NULL
  #importFrom("methods", "as")
  #importFrom("stats", "complete.cases", "filter", "median")
  #importFrom("utils", "data", "memory.limit", "read.csv", "write.csv")

  #Checking Occurrence_data format
  par_names <- c("taxon","latitude","longitude","type")

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

  # Load in protected areas

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

  # create an empty dataframe
  df <- data.frame(matrix(ncol = 2, nrow = length(Species_list)))
  colnames(df) <- c("species", "SRSin")

  for(i in seq_len(length(Species_list))){
    # pull the sdm to mask for
    for(j in seq_len(length(Raster_list))){
      if(grepl(j, i, ignore.case = TRUE)){
        sdm <- Raster_list[[j]]
      }
    };rm(j)
    # restrict protected areas to those that are present within the model threshold
    Pro_areas1 <- raster::crop(x = Pro_areas,y = sdm)
    if(raster::res(Pro_areas1)[1] != raster::res(sdm)[1]){
      Pro_areas1 <- raster::resample(x = Pro_areas1, y = sdm)
    }
    sdm[sdm == 0]<-NA
    Pro_areasSpecies <- sdm * Pro_areas1

    # filter by specific species

    occData1 <- Occurrence_data[which(Occurrence_data$taxon==Species_list[i] & !is.na(Occurrence_data$latitude)),]



    # extract values to all points
    sp::coordinates(occData1) <- ~longitude+latitude
    sp::proj4string(occData1) <- sp::CRS("+proj=longlat +datum=WGS84")
    # select all points within the SDM
    inSDM <- occData1[!is.na(raster::extract(x = sdm,y = occData1)),]
    # select all occurrences in SDM within protected area
    protectPoints <- sum(!is.na(raster::extract(x = Pro_areas1,y = inSDM)))

    # include only points that are inside of the predicted presences area.
    totalNum <- dim(inSDM)[1]
    ### all know occurrence points
    # totalNum <- nrow(occData1)

    #define SRSin
    if(protectPoints >= 0 ){
      SRSin <- 100 *(protectPoints/totalNum)
    }else{
      SRSin <- 0
    }

    # add values to empty df
    df$species[i] <- as.character(Species_list[i])
    df$SRSin[i] <- SRSin


    # number of ecoregions present in model
    if(Gap_Map==TRUE){
      # select all points within SDM outstide of protected areas
      gapP <- inSDM[is.na(raster::extract(x = Pro_areas1,y = inSDM)),]
      gapP<- sp::SpatialPoints(coords = gapP@coords)
      gap_map <- raster::rasterize(x = gapP, field = rep(x = 1, length(gapP)),
                                   y = sdm, fun='count')
      GapMapIn_list[[i]] <- gap_map
      names(GapMapIn_list[[i]] ) <- Species_list[[i]]
      }
    }
  if(Gap_Map==TRUE){
    df <- list(SRSin=df, gap_maps = GapMapIn_list )
  }else{
    df <- df
  }
return(df)
}
