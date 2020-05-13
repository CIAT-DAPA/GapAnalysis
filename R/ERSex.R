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
#' @param Ecoregions_shp A shapefile representing Ecoregionsions information with a field ECO_NUM representing Ecoregionsions Ids.
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


ERSex <- function(Species_list,Occurrence_data, Raster_list, Buffer_distance,Ecoregions_shp) {

  taxon <- NULL
  type <- NULL
  longitude <- NULL
  latitude <-NULL
  ECO_NUM <- NULL
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

        occDataG <- speciesOcc
        occDataG <- speciesOcc[which(speciesOcc$type=="G"),c("longitude","latitude")]

        # occDataG <- speciesOcc  %>%
        #   dplyr::filter(type == "G")%>%
        #   dplyr::select(longitude,latitude)

        occDataG <- occDataG[which(!is.na(occDataG$latitude)),]
          sp::coordinates(occDataG) <- ~longitude+latitude
          sp::proj4string(occDataG) <- sp::CRS("+proj=longlat +datum=WGS84")
        # select raster with species name
          for(j in seq_len(length(Raster_list))){
            if(grepl(j, i, ignore.case = TRUE)){
              sdm <- Raster_list[[j]]
            }
          }
        # convert SDM from binary to 1-NA for mask and area
        sdmMask <- sdm
        sdmMask[which(sdmMask[] == 0)] <- NA

        # buffer G points
#     buffer <- geobuffer::geobuffer_pts(xy = occData,
      buffer <- GapAnalysis::Gbuffer(xy = occDataG,
                                             dist_m = Buffer_distance,
                                             output = 'sf')
        # rasterizing and making it into a mask
        buffer_rs <- fasterize::fasterize(buffer, sdm)
        buffer_rs[!is.na(buffer_rs[])] <- 1
        buffer_rs <- buffer_rs * sdmMask
        buffer_list[[i]] <- buffer_rs
        names(buffer_list[[i]]) <- Species_list[i]
        gPoints <- sp::SpatialPoints(raster::rasterToPoints(buffer_rs))
        # extract values from Ecoregionsions to points
        raster::crs(gPoints) <- raster::crs(Ecoregions_shp)

        ecoValsG <- sp::over(x = gPoints, y = Ecoregions_shp)
        ecoValsG <- data.frame(ECO_NUM=(unique(ecoValsG$ECO_NUM)))
        ecoValsG <- ecoValsG[which(!is.na(ecoValsG) & ecoValsG>0),]

        # ecoValsG <- sp::over(x = gPoints, y = Ecoregions) %>%
        #   dplyr::distinct(ECO_NUM)%>%
        #   tidyr::drop_na(ECO_NUM) %>% #ECO_ID
        #   dplyr::filter(ECO_NUM > 0) #ECO_ID

        speciesOcc <- speciesOcc[which(!is.na(speciesOcc$latitude)),]

        # create point object from all the occurence data for the species
        sp::coordinates(speciesOcc) <- ~longitude+latitude
        sp::proj4string(speciesOcc) <- sp::CRS("+proj=longlat +datum=WGS84")
        raster::crs(speciesOcc) <- raster::crs(Ecoregions_shp)

        # number of Ecoregionsions present in model


        ecoVal <- sp::over(x = speciesOcc, y = Ecoregions_shp)
        ecoVal <- data.frame(ECO_NUM=(unique(ecoVal$ECO_NUM)))
        ecoVal <- ecoVal[which(!is.na(ecoVal) & ecoVal>0),]

        # ecoVal <- data.frame(sp::over(x = speciesOcc, y = Ecoregions))%>%
        #     dplyr::select(ECO_NUM )%>% #ECO_ID
        #     dplyr::distinct() %>%
        #     tidyr::drop_na() %>%
        #     dplyr::filter(ECO_NUM > 0) # -9998 are lakes #ECO_ID != -9998

        #calculate ERS
        ers <- min(c(100, (length(ecoValsG)/length(ecoVal))*100))
        # assign values to df
        df$species[i] <- as.character(Species_list[i])
        df$ERSex[i] <- ers
      }
  }
  df <- list(ERSex=df,buffer_list=buffer_list)
  return(df)

}
