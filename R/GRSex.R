#' @title Geographical representativeness score ex situ
#' @name GRSex
#' @description The GRSex process provides a geographic measurement of the proportion of a species’ range
#'  that can be considered to be conserved in ex situ repositories. The GRSex uses buffers (default 50 km radius)
#'  created around each G coordinate point to estimate geographic areas already well collected within the distribution
#'  models of each taxon, and then calculates the proportion of the distribution model covered by these buffers.
#' @param Occurrence_data A data frame object with the species name, geographical coordinates,
#'  and type of records (G or H) for a given species
#' @param Species_list A species list to calculate the GRSex metrics.
#' @param Raster_list A list representing the species distribution models for the species list provided
#'  loaded in raster format. This list must match the same order as the species list.
#' @param Buffer_distance Geographical distance used to create circular buffers around germplasm.
#'  Default: 50000 (50 km) around germplasm accessions (CA50)
#' @param Gap_Map Default=NULL, This option will calculate gap maps for each species analyzed and will retun a list
#'  with two slots GRSex and gap_maps
#'
#' @return This function returns a data frame with two columns:
#'
#' \tabular{lcc}{
#' species \tab Species name \cr
#' GRSex \tab GRSex value calculated\cr
#' }
#'
#' @examples
#' ##Obtaining occurrences from example
#' data(CucurbitaData)
#' Cucurbita_splist <- unique(CucurbitaData$taxon)
#' ## Obtaining rasterList object. ##
#' data(CucurbitaRasters)
#' CucurbitaRasters <- raster::unstack(CucurbitaRasters)
#' #Running GRSex
#' GRSex_df <- GRSex(Species_list = Cucurbita_splist,
#'                     Occurrence_data = CucurbitaData,
#'                     Raster_list = CucurbitaRasters,
#'                     Buffer_distance = 50000,
#'                     Gap_Map = NULL)
#'
#' @references
#' Ramirez-Villegas et al. (2010) PLOS ONE, 5(10), e13497. doi: 10.1371/journal.pone.0013497
#' Khoury et al. (2019) Ecological Indicators 98:420-429. doi: 10.1016/j.ecolind.2018.11.016
#'
#' @export
#' @importFrom sp coordinates proj4string SpatialPoints over CRS
#' @importFrom stats median
#' @importFrom fasterize fasterize
#' @importFrom raster overlay crop raster extent ncell



GRSex <- function(Species_list, Occurrence_data, Raster_list, Buffer_distance=50000, Gap_Map=NULL) {

  longitude <- NULL
  taxon <- NULL
  type <- NULL
  latitude <-NULL

  #Checking Occurrence_data format
  par_names <- c("taxon","latitude","longitude","type")

  if(identical(names(Occurrence_data),par_names)==FALSE){
    stop("Please format the column names in your dataframe as taxon,latitude,longitude,type")
  }
  #Checking if GapMapEx option is a boolean
  if(is.null(Gap_Map) | missing(Gap_Map)){ Gap_Map <- FALSE
  } else if(Gap_Map==TRUE | Gap_Map==FALSE){
    Gap_Map <- Gap_Map
  } else {
    stop("Choose a valid option for GapMap (TRUE or FALSE)")
  }

  #Checking if user is using a raster list or a raster stack
  if(class(Raster_list)=="RasterStack"){
    Raster_list <- raster::unstack(Raster_list)
  } else {
    Raster_list <- Raster_list
  }


  # create a dataframe to hold the components
  df <- data.frame(matrix(ncol = 2, nrow = length(Species_list)))
  colnames(df) <- c("species", "GRSex")

  if(Gap_Map==TRUE){
    GapMapEx_list <- list()
  }

  for(i in seq_len(length(sort(Species_list)))){
    # select species G occurrences

    OccData  <- Occurrence_data[which(Occurrence_data$taxon==Species_list[i]),]
    OccData  <- OccData [which(OccData $type == "G" & !is.na(OccData $latitude)),]
    OccData  <- OccData [,c("longitude","latitude")]

    sp::coordinates(OccData ) <- ~longitude+latitude
    sp::proj4string(OccData ) <- sp::CRS("+proj=longlat +datum=WGS84")
    # select raster with species name
    for(j in seq_len(length(Raster_list))){
      if(grepl(j, i, ignore.case = TRUE)){
        sdm <- Raster_list[[j]]
      }
    }
    # convert SDM from binary to 1-NA for mask and area
    sdmMask <- sdm
    sdmMask[sdmMask[] == 0] <- NA
    # buffer G points
    buffer <- GapAnalysis::Gbuffer(xy = OccData ,
                                       dist_m = Buffer_distance,
                                       output = 'sf')
    # rasterizing and making it into a mask
    buffer_rs <- fasterize::fasterize(buffer, sdm)
    buffer_rs[!is.na(buffer_rs[])] <- 1
    buffer_rs <- buffer_rs * sdmMask
    # calculate area of buffer
    cell_size<-raster::area(buffer_rs, na.rm=TRUE, weights=FALSE)
    cell_size<-cell_size[!is.na(cell_size)]
    gBufferRas_area<-length(cell_size)*median(cell_size)

    # calculate area of the threshold model
    cell_size<- raster::area(sdmMask, na.rm=TRUE, weights=FALSE)
    cell_size<- cell_size[!is.na(cell_size)]
    pa_spp_area <- length(cell_size)*median(cell_size)
    # calculate GRSex
    GRSex <- min(c(100, gBufferRas_area/pa_spp_area*100))

    df$species[i] <- as.character(Species_list[i])
    df$GRSex[i] <- GRSex

    #GRSex gap map

    if(Gap_Map==TRUE){
      message("Calculating gap maps for GRSex","\n")
      bf2 <- buffer_rs
      bf2[is.na(bf2),] <- 0
      gap_map <- sdmMask - bf2
      gap_map[gap_map == 0,] <- NA
      GapMapEx_list[[i]] <- gap_map
      names(GapMapEx_list[[i]] ) <- Species_list[[i]]
    }
  }
  if(Gap_Map==TRUE){
      df <- list(GRSex= df,GapMapEx_list=GapMapEx_list)
    } else {
      df <- df
    }

  return(df)
}
