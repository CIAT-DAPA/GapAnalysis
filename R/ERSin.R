#' @title Ecological representativeness score in situ
#' @name ERSin
#' @description The ERSin process provides an ecological measurement of the proportion of a species range
#'  that can be considered to be conserved in protected areas. The ERSin calculates the proportion of ecoregions
#'  encompassed within the range of the taxon located inside protected areas to the ecoregions encompassed
#'  within the total area of the distribution model, considering comprehensive conservation to have been accomplished
#'  only when every ecoregion potentially inhabited by a species is included within the distribution of the species
#'  located within a protected area.
#'  This function uses a thresholded species distribution model, an ecoregions file, and a protected areas file
#' @param Occurrence_data A data frame object with the species name, geographical coordinates,
#'  and type of records (G or H) for a given species
#' @param Species_list A vector of characters with the species names to calculate the GRSex metrics.
#' @param Raster_list A list of rasters representing the species distribution models for the species list provided
#'  in \var{Species_list}. The order of rasters in this list must match the same order as \var{Species_list}.
#' @param Pro_areas A raster file representing protected areas information.
#'  If Pro_areas=NULL the function will use a protected area raster file
#'  provided for your use after run GetDatasets()
#' @param Ecoregions_shp A shapefile representing Ecoregions_shp information with a field ECO_NUM representing Ecoregions_shp Ids.
#'  If Ecoregions_shp=NULL the funtion will use a protected area raster file provided for your use after run GetDatasets()
#' @param Gap_Map logical, if \code{TRUE} the function will calculate gap maps for each species analyzed and will return a list
#'  with two slots ERSin and gap_maps
#' @return This function returns a dataframe as main result with two columns:
#'
#' \tabular{lcc}{
#' species \tab Species name \cr
#' ERSin \tab ERSin value calculated\cr
#' }
#'
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
#' ##Obtaining ecoregions shapefile
#' data(ecoregions)
#' #Running ERSin
#' ERSin_df <- ERSin(Species_list = Cucurbita_splist,
#'                    Occurrence_data = CucurbitaData,
#'                    Raster_list = CucurbitaRasters,
#'                    Pro_areas= ProtectedAreas,
#'                    Ecoregions_shp=ecoregions,
#'                    Gap_Map=FALSE)
#'
#'@references
#
#' Khoury et al. (2019) Ecological Indicators 98:420-429. doi: 10.1016/j.ecolind.2018.11.016
#'
#' @export
#' @importFrom stats median
#' @importFrom raster raster crop area shapefile

ERSin <- function(Species_list,Occurrence_data,Raster_list,Pro_areas=NULL,Ecoregions_shp=NULL,Gap_Map=FALSE) {

  taxon <- NULL
  type <- NULL
  longitude <- NULL
  ECO_ID_U <- NULL
  SdmMask <- NULL


  #Checking Occurrence_data format
  par_names <- c("species","latitude","longitude","type")


  if(missing(Occurrence_data)){
    stop("Please add a valid data frame with columns: species, latitude, longitude, type")
  }


  if(isFALSE(identical(names(Occurrence_data),par_names))){
    stop("Please format the column names in your dataframe as species, latitude, longitude, type")
  }


  #Checking if Gap_Map option is a boolean or if the parameter is missing left Gap_Map as FALSE
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


  ## ERSin analyzes how well protected areas cover the distribution model with regard to ecosystems covered
  df <- data.frame(matrix(ncol=2, nrow = length(Species_list)))
  colnames(df) <- c("species", "ERSin")
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


  if(isTRUE(Gap_Map)){
    GapMapIn_list <- list()
  }

  for(i in seq_len(length(Species_list))){

    # select threshold map for a given species
    for(j in seq_len(length(Raster_list))){
      if(grepl(j, i, ignore.case = TRUE)){
        sdm <- Raster_list[[j]]
      }
    d1 <- Occurrence_data[Occurrence_data$species == Species_list[i],]
    test <- GapAnalysis::paramTest(d1, sdm)
    if(isTRUE(test[1])){
       stop(paste0("No Occurrence data exists, but and SDM was provide. Please check your occurrence data input for ", Species_list[i]))
  }

  };rm(j)

    if(isFALSE(test[2])){
      df$species[i] <- as.character(Species_list[i])
      df$GRSex[i] <- 0
      warning(paste0("Either no occurrence data or SDM was found for species ", as.character(Species_list[i]),
                     " the conservation metric was automatically assigned 0"))
    }else{
    # mask protected areas to threshold
    Pro_areas1 <- raster::crop(x = Pro_areas, y=sdm)
    if(raster::res(Pro_areas1)[1] != raster::res(sdm)[1]){
      Pro_areas1 <- raster::resample(x = Pro_areas1, y = sdm)
    }
    sdm[sdm[] != 1] <- NA
    Pro_areas1 <- sdm * Pro_areas1

    #convert protect area to points
    protectPoints <- sp::SpatialPoints(raster::rasterToPoints(Pro_areas1))
    # extract the Ecoregions_shp values to the points
    suppressWarnings(raster::crs(protectPoints) <- raster::crs(Ecoregions_shp))
    suppressWarnings(ecoValsPro <- sp::over(x = protectPoints, y = Ecoregions_shp))
    ecoValsPro <- data.frame(ECO_ID_U=(unique(ecoValsPro$ECO_ID_U)))
    ecoValsPro <- ecoValsPro[which(!is.na(ecoValsPro) & ecoValsPro>0),]
    ecoInProt <- length(ecoValsPro)

    # extract ecoregions values present in predicted presence area
    predictedPresence <- sp::SpatialPoints(raster::rasterToPoints(sdm))
    raster::crs(predictedPresence) <- raster::crs(Ecoregions_shp)
    suppressWarnings( ecoVal <- sp::over(x = predictedPresence, y = Ecoregions_shp))
    ecoVal <- data.frame(ECO_ID_U=(unique(ecoVal$ECO_ID_U)))
    ecoVal <- ecoVal[which(!is.na(ecoVal) & ecoVal>0),]
    # number of Ecoregions_shpions in modeling area
    ecoInSDM <- length(ecoVal)

    #clause for 9 in protected area
    if(ecoInProt == 0){
      df$species[i] <- as.character(Species_list[i])
      df$ERSin[i] <- 0
    }else{
      # calculate ERSin
      ERSin <- min(c(100, (ecoInProt/ecoInSDM)*100))
      df$species[i] <- as.character(Species_list[i])
      df$ERSin[i] <- ERSin
    }
    if(isTRUE(Gap_Map)){
      message(paste0("Calculating ERSin gap map for ",as.character(Species_list[i])),"\n")

      # ERSin Gap Map
      # select all ecoregions present in ecoVal (all points) but absent in ecoValG (g buffers)
      ecoGap <- ecoVal[!ecoVal %in% ecoValsPro]
      if(length(ecoGap) == 0){
        r1 <- raster::raster()
        raster::extent(r1) <- raster::extent(sdm)
        raster::values(r1) <- NA
        GapMapIn_list[[i]] <- r1
        }else{
        SdmMask <-  sdm
        SdmMask[which(SdmMask[]!=1)] <- NA
        # pull selected ecoregions and mask to presence area of the model
        eco2 <- Ecoregions_shp[Ecoregions_shp$ECO_ID_U %in% ecoGap,]
        #convert to sf object for conversion using fasterize
        eco2a <- sf::st_as_sf(eco2, SdmMask)
        # generate a ecoregion raster keeping the unique id.
        eco3 <- fasterize::fasterize(eco2a, SdmMask, field = "ECO_ID_U")
        # mask so only locations within the predicted presence area is included.
        gap_map <- eco3 * SdmMask
        GapMapIn_list[[i]] <- gap_map
        names(GapMapIn_list[[i]] ) <- Species_list[[i]]
      }
    }
  }
}
  if(isTRUE(Gap_Map)){
    df <- list(ERSin=df, gap_maps = GapMapIn_list )
  }else{
    df <- df#list(ERSin=df)
  }

  return(df)
}
