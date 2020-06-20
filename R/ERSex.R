#' @title Ecological representativeness score ex situ
#' @name ERSex
#' @description The ERSex process provides an ecological measurement of the proportion of a species
#'  range that can be considered to be conserved in ex situ repositories. The ERSex calculates the
#'  proportion of terrestrial ecoregions (The Nature Conservancy Geospatial Conservation Atlas 2019)
#'  represented within the G buffered areas out of the total number of ecoregions occupied by the distribution model.
#' @param Occurrence_data A data frame object with the species name, geographical coordinates,
#'  and type of records (G or H) for a given species
#' @param Species_list A species list to calculate the ERSex metrics.
#' @param Raster_list A list representing the species distribution models for the species list provided
#'  loaded in raster format. This list must match the same order as the species list.
#' @param Buffer_distance Geographical distance used to create circular buffers around germplasm site of collection points.
#'  Default: 50000 (50 km) around germplasm accession coordinates (CA50)
#' @param Ecoregions_shp A shapefile representing Ecoregions information with a field ECO_ID_U representing Ecoregions Ids.
#'  If Ecoregions=NULL the function will use a shapefile provided for use after running GetDatasets()
#' @param Gap_Map Default=NULL, This option will calculate gap maps for each species analyzed and will return a list
#' with two slots ERSex and gap_maps, or three slots ERSex, buffer_list, and gap_maps when Gap_Map=TRUE
#' @return This function returns a dataframe as main result with two columns:
#'
#' \tabular{lcc}{
#' species \tab Species name \cr
#' ERSex \tab ERSex value calculated\cr
#' }
#'
#' @examples
#' ##Obtaining occurrences from example
#' data(CucurbitaData)
#' Cucurbita_splist <- unique(CucurbitaData$taxon)
#' ## Obtaining rasterList object. ##
#' data(CucurbitaRasters)
#' CucurbitaRasters <- raster::unstack(CucurbitaRasters)
#' ##Obtaining ecoregions shapefile
#' data(ecoregions)
#' #Running ERSex
#' ERSex_df <- ERSex(Species_list = Cucurbita_splist,
#'                     Occurrence_data = CucurbitaData,
#'                     Raster_list = CucurbitaRasters,
#'                     Buffer_distance = 50000,
#'                     Ecoregions_shp=ecoregions,
#'                     Gap_Map=NULL)
#'
#' @references
#'
#' Castañeda-Álvarez et al. (2016) Nature Plants 2(4):16022. doi: 10.1038/nplants.2016.22
#' Khoury et al. (2019) Ecological Indicators 98:420-429. doi: 10.1016/j.ecolind.2018.11.016
#' The Nature Conservancy Geospatial Conservation Atlas. 2019. Terrestrial Ecoregions
#'
#'
#' @export
#' @importFrom raster shapefile rasterToPoints crs
#' @importFrom fasterize fasterize
#' @importFrom sp coordinates proj4string SpatialPoints over CRS
#' @importFrom sf st_as_sf


ERSex <- function(Species_list,Occurrence_data, Raster_list, Buffer_distance=50000,Ecoregions_shp=NULL,Gap_Map=NULL){

  taxon <- NULL
  type <- NULL
  longitude <- NULL
  latitude <-NULL
  ECO_ID_U <- NULL
  nc <- NULL
  ecoVal <- NULL
  ecoValsPro <- NULL

  buffer_list <- list()

  #Checking Occurrence_data format
  par_names <- c("taxon","latitude","longitude","type")

  if(missing(Occurrence_data)){
    stop("Please add a valid data frame with columns: taxon,latitude,longitude,type")
  }


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
  # load in ecoregions dataset
  # Load in ecoregions shp
  #print(missing(Ecoregions_shp))
  if(is.null(Ecoregions_shp)){
    if(file.exists(system.file("data/preloaded_data/ecoRegion/tnc_terr_ecoregions.shp",package = "GapAnalysis"))){
      Ecoregions_shp <- raster::shapefile(system.file("data/preloaded_data/ecoRegion/tnc_terr_ecoregions.shp", package = "GapAnalysis"),encoding = "UTF-8")
    } else {
      stop("Ecoregions file is not available yet. Please run the function GetDatasets() and try again")
    }
  } else{
    Ecoregions_shp <- Ecoregions_shp
  }

  if(Gap_Map==TRUE){
    GapMapEx_list <- list()
  }

    # maybe this directly downloads an element from the dataverse

  # generate a dataframe to store the output values
  df <- data.frame(matrix(ncol = 2, nrow = length(Species_list)))
  colnames(df) <- c("species", "ERSex")

  # loop through all species calculate ERSex and produce map
  for(i in seq_len(length(Species_list))){
    speciesOcc <- Occurrence_data[which(Occurrence_data$taxon==Species_list[i]),]
    if(length(speciesOcc$type == "G") == 0){
      df$species[i] <- Species_list[i]
      df$ERSex[i] <- 0
      }else{

        OccDataG <- speciesOcc
        OccDataG <- speciesOcc[which(speciesOcc$type=="G"),c("longitude","latitude")]


        OccDataG <- OccDataG[which(!is.na(OccDataG$latitude)),]

          sp::coordinates(OccDataG) <- ~longitude+latitude
          sp::proj4string(OccDataG) <- sp::CRS("+proj=longlat +datum=WGS84")
        # select raster with species name
          for(j in seq_len(length(Raster_list))){
            if(grepl(j, i, ignore.case = TRUE)){
              sdm <- Raster_list[[j]]
            }
          }
        # convert SDM from binary to 1-NA for mask and area
        SdmMask <- sdm
        SdmMask[which(SdmMask[] == 0)] <- NA

        # buffer G points
        buffer <- GapAnalysis::Gbuffer(xy = OccDataG,
                                             dist_m = Buffer_distance,
                                             output = 'sf')
        # rasterizing and making it into a mask
        buffer_rs <- fasterize::fasterize(buffer, sdm)
        buffer_rs[!is.na(buffer_rs[])] <- 1
        buffer_rs <- buffer_rs * SdmMask
        buffer_list[[i]] <- buffer_rs
        names(buffer_list[[i]]) <- Species_list[i]
        gPoints <- sp::SpatialPoints(raster::rasterToPoints(buffer_rs))
        # extract values from ecoregions to points
        raster::crs(gPoints) <- raster::crs(Ecoregions_shp)

        ecoValsG <- sp::over(x = gPoints, y = Ecoregions_shp)
        ecoValsG <- data.frame(ECO_ID_U=(unique(ecoValsG$ECO_ID_U)))
        ecoValsG <- ecoValsG[which(!is.na(ecoValsG) & ecoValsG>0),]

        # extract values from ecoregion to predicted presences points
        predictedPresence <- sp::SpatialPoints(raster::rasterToPoints(SdmMask))
        raster::crs(predictedPresence) <- raster::crs(Ecoregions_shp)
        ecoVals <- sp::over(x = predictedPresence, y = Ecoregions_shp)
        ecoVals <- data.frame(ECO_ID_U=(unique(ecoVals$ECO_ID_U)))
        ecoVals <- ecoVals[which(!is.na(ecoVals) & ecoVals>0),]

        #calculate ERSex
        ERSex <- min(c(100, (length(ecoValsG)/length(ecoVals))*100))
        # assign values to df
        df$species[i] <- as.character(Species_list[i])
        df$ERSex[i] <- ERSex

        # number of ecoregions present in model
        if(Gap_Map==TRUE){
          message(paste0("Calculating ERSex gap map for ",as.character(Species_list[i])),"\n")

          # ERSex Gap Map
          # select all ecoregions present in ecoVal(all points) but absent in ecoValG(g buffers)
          ecoGap <- ecoVals[!ecoVals %in% ecoValsG]
          if(length(ecoGap) == 0){
            GapMapEx_list[[i]] <- paste0("All ecoregions within the model are within ", Buffer_distance,
          "km of G occurrence. There are no gaps")

          }else{
          # pull selected ecoregions and mask to presence area of the model
          eco2 <- Ecoregions_shp[Ecoregions_shp$ECO_ID_U %in% ecoGap,]
          #convert to sf object for conversion using fasterize
          eco2a <- sf::st_as_sf(eco2, SdmMask)
          # generate a ecoregion raster keeping the unique id.
          eco3 <- fasterize::fasterize(eco2a, SdmMask, field = "ECO_ID_U")
          # mask so only locations within the predicted presence area is included.
          gap_map <- eco3 * SdmMask
          GapMapEx_list[[i]] <- gap_map
          names(GapMapEx_list[[i]] ) <- Species_list[[i]]
        }
      }
    }
  }
  if(Gap_Map==TRUE){
    df <- list(ERSex=df,buffer_list=buffer_list, gap_maps = GapMapEx_list )
  }else{
    df <- list(ERSex=df,buffer_list=buffer_list)
  }
  return(df)
}
