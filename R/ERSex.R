#' @title Environmental representativeness score estimation (Ex-situ conservation)
#' @name ERSex
#' @description This function performs an estimation of the environmental representativeness
#'  score for ex-situ gap analysis (ERSex) using Ramirez-Villegas et al., (2010) methodology.
#' ERSex is calculated as:
#' \deqn{ERSex = min(100,(Number of Ecoregionsions with 50km of G Occurrences /
#'  Number of Ecoregionsions Present within the Predict habitat)*100)}
#'
#' @param Occurrence_data A data frame object with the species name, geographical coordinates,
#'  and type of records (G or H) for a given species
#' @param Species_list An species list to calculate the ERSex metrics.
#' @param Raster_list A list representing the species distribution models for the species list provided
#'  loaded in raster format. This list must match the same order of the species list.
#' @param Buffer_distance Geographical distance used to create circular buffers around germplasm.
#'  Default: 50000 that is 50 km around germplasm accessions (CA50)
#' @param Ecoregions_shp A shapefile representing Ecoregionsions information with a field ECO_ID_U representing Ecoregionsions Ids.
#'  If Ecoregions=NULL the function will use a shapefile provided for your use after run GetDatasets()
#'
#' @return This function returns a data frame with two columns:
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
#' ##Obtaining Ecoregionsions shapefile
#' data(ecoregions)
#'
#' ERSex_df <- ERSex(Species_list = Cucurbita_splist,
#'                     Occurrence_data = CucurbitaData,
#'                     Raster_list = CucurbitaRasters,
#'                     Buffer_distance = 50000,
#'                     Ecoregions_shp=ecoregions)
#'
#' @references
#'
#' Ramirez-Villegas, J., Khoury, C., Jarvis, A., Debouck, D. G., & Guarino, L. (2010).
#' A Gap Analysis Methodology for Collecting Crop Genepools: A Case Study with Phaseolus Beans.
#' PLOS ONE, 5(10), e13497. Retrieved from https://doi.org/10.1371/journal.pone.0013497
#'
#' Khoury, C. K., Amariles, D., Soto, J. S., Diaz, M. V., Sotelo, S., Sosa, C. C., … Jarvis, A. (2019).
#' Comprehensiveness of conservation of useful wild plants: An operational indicator for biodiversity
#' and sustainable development targets. Ecological Indicators. https://doi.org/10.1016/j.ecolind.2018.11.016
#' @export
#' @importFrom raster shapefile rasterToPoints crs
#' @importFrom  fasterize fasterize
#' @importFrom sp coordinates proj4string SpatialPoints over CRS




ERSex <- function(Species_list,Occurrence_data, Raster_list, Buffer_distance=50000,Ecoregions_shp=NULL, Gap_Map=NULL) {

  taxon <- NULL
  type <- NULL
  longitude <- NULL
  latitude <-NULL
  ECO_ID_U <- NULL
  nc <- NULL
  buffer_list <- list()
#load packages
# suppressMessages(require(sp))
# suppressMessages(require(raster))
# suppressMessages(require(dplyr))
# suppressMessages(require(tidyr))

#importFrom("methods", "as")
#importFrom("stats", "complete.cases", "filter", "median")
#importFrom("utils", "data", "memory.limit", "read.csv", "write.csv")

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
  # load in ecoregions dataset
  # Load in ecoregions shp
  if(is.null(Ecoregions_shp)){
    if(file.exists(system.file("data/preloaded_data/ecoRegion/tnc_terr_ecoregions.shp",package = "GapAnalysis"))){
      Ecoregions_shp <- raster::shapefile(system.file("data/preloaded_data/ecoRegion/tnc_terr_ecoregions.shp", package = "GapAnalysis"),encoding = "UTF-8")
    } else {
      stop("Ecoregions file is not available yet. Please run the function GetDatasets() and try again")
    }
  } else{
    Ecoregions_shp <- Ecoregions_shp
  }
  
  if(Gap_Map==T){
    GapMapEx_list <- list()
  }

    # maybe this directly downloads an element from the dataverse

  # generate a dataframe to store the output values
  df <- data.frame(matrix(ncol = 2, nrow = length(Species_list)))
  colnames(df) <- c("species", "ERSex")

  # loop through all species
  for(i in seq_len(length(Species_list))){
    speciesOcc <- Occurrence_data[which(Occurrence_data$taxon==Species_list[i]),]
    # speciesOcc <- Occurrence_data %>%
    #   #tidyr::drop_na(longitude)%>%
    #   dplyr::filter(taxon == Species_list[i])
    if(length(speciesOcc$type == "G") == 0){
      df$species[i] <- Species_list[i]
      df$ERSex[i] <- 0
      }else{

        OccDataG <- speciesOcc
        OccDataG <- speciesOcc[which(speciesOcc$type=="G"),c("longitude","latitude")]

        # OccDataG <- speciesOcc  %>%
        #   dplyr::filter(type == "G")%>%
        #   dplyr::select(longitude,latitude)

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
#     buffer <- geobuffer::geobuffer_pts(xy = occData,
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
        # extract values from Ecoregionsions to points
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
        
        #calculate ERS
        ERSex <- min(c(100, (length(ecoValsG)/length(ecoVals))*100))
        # assign values to df
        df$species[i] <- as.character(Species_list[i])
        df$ERSex[i] <- ERSex
    
        # number of Ecoregionsions present in model
        if(Gap_Map==T){
          cat("Calculating gap maps for ERSex gap analysis","\n")
          
          # ERSex Gap Map 
          # select all ecoregions present in ecoVal(all points) but absent in ecoValG(g buffers)
          ecoGap <- ecoVals[!ecoVals %in% ecoValsG]
          ecoGap <- ecoVal[!ecoVal %in% ecoValsPro]
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
  if(Gap_Map==T){
    df <- list(ERSex=df,buffer_list=buffer_list, gap_maps = GapMapEx_list )
  }else{
    df <- list(ERSex=df,buffer_list=buffer_list)
  }
  return(df)
}
