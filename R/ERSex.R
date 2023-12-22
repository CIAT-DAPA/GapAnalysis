#' @title Ecological representativeness score ex situ
#' @name ERSex
#' @description The ERSex process provides an ecological measurement of the proportion of a species
#'  range that can be considered to be conserved in ex situ repositories. The ERSex calculates the
#'  proportion of terrestrial ecoregions (The Nature Conservancy Geospatial Conservation Atlas 2019)
#'  represented within the G buffered areas out of the total number of ecoregions occupied by the distribution model.
#' @param Occurrence_data A data frame object with the species name, geographical coordinates,
#'  and type of records (G or H) for a given species
#' @param Species_list A vector of characters with the species names to calculate the GRSex metrics.
#' @param Raster_list A list of rasters representing the species distribution models for the species list provided
#'  in \var{Species_list}. The order of rasters in this list must match the same order as \var{Species_list}.
#' @param Buffer_distance Geographical distance used to create circular buffers around germplasm.
#'  Default: 50000 (50 km) around germplasm accessions (CA50)
#' @param Ecoregions_shp A shapefile representing Ecoregions information with a field ECO_ID_U representing Ecoregions Ids.
#'  If Ecoregions=NULL the function will use a shapefile provided for use after running GetDatasets()
#' @param Gap_Map logical, if \code{TRUE} the function will calculate gap maps for each species analyzed and
#'  will return a list with with two slots ERSex and gap_maps, or three slots ERSex, buffer_list, and gap_maps
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
#' Cucurbita_splist <- unique(CucurbitaData$species)
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
#'                     Gap_Map=FALSE)
#'
#' @references
#'
#' Castaneda-Alvarez et al. (2016) Nature Plants 2(4):16022. doi: 10.1038/nplants.2016.22
#' Khoury et al. (2019) Ecological Indicators 98:420-429. doi: 10.1016/j.ecolind.2018.11.016
#' The Nature Conservancy Geospatial Conservation Atlas. 2019. Terrestrial Ecoregions
#'
#'
#' @export
#' @importFrom raster shapefile rasterToPoints crs projection
#' @importFrom fasterize fasterize
#' @importFrom sp coordinates proj4string SpatialPoints over CRS
#' @importFrom sf st_as_sf


ERSex <- function(Species_list,Occurrence_data, Raster_list, Buffer_distance=50000,Ecoregions_shp=NULL,Gap_Map=FALSE){

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
  par_names <- c("species","latitude","longitude","type")

  if(missing(Occurrence_data)){
    stop("Please add a valid data frame with columns: species, latitude, longitude, type")
  }


  if(isFALSE(identical(names(Occurrence_data),par_names))){
    stop("Please format the column names in your dataframe as species, latitude, longitude, type")
  }

  #Checking if GapMapEx option is a boolean or if the parameter is missing left Gap_Map as FALSE
  if(is.null(Gap_Map) | missing(Gap_Map)){ Gap_Map <- FALSE
  } else if(isTRUE(Gap_Map) | isFALSE(Gap_Map)){
    Gap_Map <- Gap_Map
  } else {
    stop("Choose a valid option for GapMap (TRUE or FALSE)")
  }

  #Checking if user is using a raster list or a raster stack
  if (isTRUE("RasterStack" %in% class(Raster_list))) {
    Raster_list <- raster::unstack(Raster_list)
  } else {
    Raster_list <- Raster_list
  }
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

  if(isTRUE(Gap_Map)){
    GapMapEx_list <- list()
  }


  # generate a dataframe to store the output values
  df <- data.frame(matrix(ncol = 2, nrow = length(Species_list)))
  colnames(df) <- c("species", "ERSex")

  # loop through all species calculate ERSex and produce map
  for(i in seq_len(length(Species_list))){

    speciesOcc <- Occurrence_data[which(Occurrence_data$species==Species_list[i]),]

    if(length(speciesOcc$type == "G") == 0){
      df$species[i] <- Species_list[i]
      df$ERSex[i] <- 0
    }else{

      OccDataG <- speciesOcc
      OccDataG <- speciesOcc[which(speciesOcc$type=="G"),c("longitude","latitude")]


      OccDataG <- OccDataG[which(!is.na(OccDataG$latitude) & !is.na(OccDataG$longitude)),]


      # # select raster with species name
      for(j in seq_len(length(Raster_list))){
        if(grepl(j, i, ignore.case = TRUE)){
          sdm <- Raster_list[[j]]
        }
        d1 <- Occurrence_data[Occurrence_data$species == Species_list[i],]
        test <- GapAnalysis::ParamTest(d1, sdm)
        if(isTRUE(test[1])){
          stop(paste0("No Occurrence data exists, but and SDM was provide. Please check your occurrence data input for ", Species_list[i]))
        }

      };rm(j)

      if(isFALSE(test[2])){
        df$species[i] <- as.character(Species_list[i])
        df$ERSex[i] <- 0
        warning(paste0("Either no occurrence data or SDM was found for species ", as.character(Species_list[i]),
                       " the conservation metric was automatically assigned 0"))
      } else {


        #sp::coordinates(OccDataG) <- ~longitude+latitude

        #Checking raster projection and assumming it for the occurrences dataframe shapefile
        if(is.na(raster::crs(sdm))){
          warning("No coordinate system was provided, assuming  +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0","\n")
          raster::projection(sdm) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
        }
        # suppressWarnings(sp::proj4string(OccDataG) <- sp::CRS(raster::projection(sdm)))
        # convert SDM from binary to 1-NA for mask and area
        SdmMask <- sdm
        SdmMask[which(SdmMask[] != 1)] <- NA

        # buffer G points
        buffer <- Gbuffer(xy = OccDataG,
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
        suppressWarnings(raster::crs(gPoints) <- raster::crs(raster::projection(Ecoregions_shp)))

        ecoValsG <- suppressWarnings(sp::over(x = gPoints, y = Ecoregions_shp))
        ecoValsG <- data.frame(ECO_ID_U=(unique(ecoValsG$ECO_ID_U)))
        ecoValsG <- ecoValsG[which(!is.na(ecoValsG) & ecoValsG>0),]

        # extract values from ecoregion to predicted presences points
        predictedPresence <- sp::SpatialPoints(raster::rasterToPoints(SdmMask))
        raster::crs(predictedPresence) <- raster::crs(Ecoregions_shp)
        ecoVals <- suppressWarnings(sp::over(x = predictedPresence, y = Ecoregions_shp))
        ecoVals <- data.frame(ECO_ID_U=(unique(ecoVals$ECO_ID_U)))
        ecoVals <- ecoVals[which(!is.na(ecoVals) & ecoVals>0),]

        #calculate ERSex
        ERSex <- min(c(100, (length(ecoValsG)/length(ecoVals))*100))
        # assign values to df
        df$species[i] <- as.character(Species_list[i])
        df$ERSex[i] <- ERSex

        # number of ecoregions present in model
        if(isTRUE(Gap_Map)){
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
  }
    if(isTRUE(Gap_Map)){
      df <- list(ERSex=df,buffer_list=buffer_list, gap_maps = GapMapEx_list )
    }else{
      df <- list(ERSex=df,buffer_list=buffer_list)
    }

  return(df)
}
