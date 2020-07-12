#' @title Sampling representativeness score in situ
#' @name SRSin
#' @description The SRSin process calculates the proportion of all occurrences of a taxon falling within
#' the distribution model that also fall within a protected area
#' @param Occurrence_data A data frame object with the species name, geographical coordinates,
#'  and type of records (G or H) for a given species
#' @param Species_list A vector of characters with the species names to calculate the GRSex metrics.
#' @param Raster_list A list of rasters representing the species distribution models for the species list provided
#'  in \var{Species_list}. The order of rasters in this list must match the same order as \var{Species_list}.
#' @param Pro_areas A raster file representing protected areas information.
#'  If Pro_areas=NULL the function will use a protected area raster file
#'  provided for your use after run GetDatasets()
#' @param Gap_Map logical, if \code{TRUE} the function will calculate gap maps for each species analyzed and will return a list
#'   with two slots SRSin and gap_maps
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
#' Cucurbita_splist <- unique(CucurbitaData$species)
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
#'                     Gap_Map=FALSE)
#'
#'@references
#'
#' Khoury et al. (2019) Diversity and Distributions 26(2):209-225. doi: 10.1111/DDI.13008.
#'
#' @export
#' @importFrom raster raster crop projection


SRSin <- function(Species_list, Occurrence_data, Raster_list,Pro_areas=NULL, Gap_Map=FALSE){

  taxon <- NULL
  longitude <- NULL

  #Checking Occurrence_data format
  par_names <- c("species","latitude","longitude","type")

  if(missing(Occurrence_data)){
    stop("Please add a valid data frame with columns: species, latitude, longitude, type")
  }

  if(identical(names(Occurrence_data),par_names)==FALSE){
    stop("Please format the column names in your dataframe as species, latitude, longitude, type")
  }

  #Checking if user is using a raster list or a raster stack
  if (isTRUE("RasterStack" %in% class(Raster_list))) {
    Raster_list <- raster::unstack(Raster_list)
  } else {
    Raster_list <- Raster_list
  }


  #Checking if Gap_Map option is a boolean  or if the parameter is missing left Gap_Map as FALSE
  if(is.null(Gap_Map) | missing(Gap_Map)){ Gap_Map <- FALSE
  } else if(isTRUE(Gap_Map) | isFALSE(Gap_Map)){
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

  if(isTRUE(Gap_Map)){
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
      # restrict protected areas to those that are present within the model threshold
      Pro_areas1 <- raster::crop(x = Pro_areas,y = sdm)
      if(raster::res(Pro_areas1)[1] != raster::res(sdm)[1]){
        Pro_areas1 <- raster::resample(x = Pro_areas1, y = sdm)
      }
      sdm[sdm[] != 1] <- NA
      Pro_areasSpecies <- sdm * Pro_areas1

      # filter by specific species

      occData1 <- Occurrence_data[which(Occurrence_data$species==Species_list[i] & !is.na(Occurrence_data$latitude) &  !is.na(Occurrence_data$longitude) ),]

       # extract values to all points
      sp::coordinates(occData1) <- ~longitude+latitude
      #Checking raster projection and assumming it for the occurrences dataframe shapefile
      if(is.na(raster::crs(sdm))){
        print("No coordinate system was provided, assuming  +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0","\n")
        raster::projection(sdm) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
      }
      suppressWarnings(sp::proj4string(occData1) <- sp::CRS(raster::projection(sdm)))

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
      if(isTRUE(Gap_Map)){
        message(paste0("Calculating SRSin gap map for ",as.character(Species_list[i])),"\n")

        # select all points within SDM outstide of protected areas
        gapP <- inSDM[is.na(raster::extract(x = Pro_areas1,y = inSDM)),]
        gapP<- sp::SpatialPoints(coords = gapP@coords)
        gap_map <- raster::rasterize(x = gapP, field = rep(x = 1, length(gapP)),
                                     y = sdm, fun='count')
        gap_map[is.na(gap_map),] <- 0
        sdm[sdm[] !=1] <- NA
        gap_map <- sdm * gap_map
        GapMapIn_list[[i]] <- gap_map
        names(GapMapIn_list[[i]] ) <- Species_list[[i]]
        }
      }
  }

    if(isTRUE(Gap_Map)){
      df <- list(SRSin=df, gap_maps = GapMapIn_list )
    }else{
      df <- df
    }


return(df)
}
