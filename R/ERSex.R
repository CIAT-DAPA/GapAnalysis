#' @title Environmental representativeness score estimation (Ex-situ conservation)
#' @name ERSex
#' @description This function performs an estimation of the environmental representativeness score for ex-situ gap analysis (ERSex) using Ramirez-Villegas et al., (2010) methodology.
#' ERSex is calculated as:
#' \deqn{ERSex = min(100,(Number of ecoregions with 50km of G Occurrences / Number of Ecoregions Present within the Predict habitat)*100)}
#'
#' @param occurrenceData A data frame object with the species name, geographical coordinates, and type of records (G or H) for a given species
#' @param species_list An species list to calculate the ERSex metrics.
#' @param raster_list A list representing the species distribution models for the species list provided loaded in raster format. This list must match the same order of the species list.
#' @param bufferDistance Geographical distance used to create circular buffers around germplasm. Default: 50000 that is 50 km around germplasm accessions (CA50)
#' @param ecoReg A shapefile representing ecoregions information with a field ECO_NUM representing ecoregions Ids. If ecoReg=NULL the function will use a shapefile
#'  provided for your use after run GetDatasets()
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
#' speciesList <- unique(CucurbitaData$taxon)
#' ## Obtaining rasterList object. ##
#' data(CucurbitaRasters)
#' CucurbitaRasters <- raster::unstack(CucurbitaRasters)
#' ##Obtaining ecoregions shapefile
#' data(ecoregions)
#'
#' ERSex_df <- ERSex(species_list = speciesList,
#'                     occurrenceData = CucurbitaData,
#'                     raster_list = CucurbitaRasters,
#'                     bufferDistance = 50000,
#'                     ecoReg=ecoregions)
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
#' @importFrom magrittr %>%
#' @importFrom dplyr filter mutate mutate_if summarize group_by
#' @importFrom tidyr drop_na
#' @importFrom raster shapefile rasterToPoints crs
#' @importFrom  fasterize fasterize
#' @import sp


ERSex <- function(species_list,occurrenceData, raster_list, bufferDistance,ecoReg) {

  taxon <- NULL
  type <- NULL
  longitude <- NULL
  latitude <-NULL
  ECO_NUM <- NULL

#load packages
# suppressMessages(require(sp))
# suppressMessages(require(raster))
# suppressMessages(require(dplyr))
# suppressMessages(require(tidyr))

#importFrom("methods", "as")
#importFrom("stats", "complete.cases", "filter", "median")
#importFrom("utils", "data", "memory.limit", "read.csv", "write.csv")

  # load in ecoregion dataset
  # Load in ecoregions shp
  if(is.null(ecoReg)){
    if(file.exists(system.file("data/preloaded_data/ecoRegion/tnc_terr_ecoregions.shp",package = "GapAnalysis"))){
      ecoReg <- raster::shapefile(system.file("data/preloaded_data/ecoRegion/tnc_terr_ecoregions.shp", package = "GapAnalysis"),encoding = "UTF-8")
    } else {
      stop("Ecoregions file is not available yet. Please run the function preparingDatasets() and try again")
    }
  } else{
    ecoReg = ecoReg
  }

  ecoReg@data <-  ecoReg@data %>%
    dplyr::mutate_if(is.character, iconv, to = 'UTF-8')
  # maybe this directly downloads an element from the dataverse

  # generate a dataframe to store the output values
  df <- data.frame(matrix(ncol = 2, nrow = length(species_list)))
  colnames(df) <- c("species", "ERSex")

  # loop through all species
  for(i in 1:length(species_list)){
    speciesOcc <- occurrenceData %>%
      #tidyr::drop_na(longitude)%>%
      dplyr::filter(taxon == species_list[i])
    if(length(speciesOcc$type == "G") == 0){
      df$species[i] <- species_list[i]
      df$ERSex[i] <- 0
      }else{
        occDataG <- speciesOcc  %>%
          dplyr::filter(type == "G")%>%
          dplyr::select(longitude,latitude)

        occDataG <- occDataG[which(!is.na(occDataG$latitude)),]
          sp::coordinates(occDataG) <- ~longitude+latitude
          sp::proj4string(occDataG) <- sp::CRS("+proj=longlat +datum=WGS84")
        # select raster with species name
          for(j in 1:length(raster_list)){
            if(grepl(j, i, ignore.case = TRUE)){
              sdm <- raster_list[[j]]
            }
          }
        # convert SDM from binary to 1-NA for mask and area
        sdmMask <- sdm
        sdmMask[which(sdmMask[] == 0)] <- NA

        # buffer G points
#    buffer <- geobuffer::geobuffer_pts(xy = occData,
    buffer <- GapAnalysis::Gbuffer(xy = occDataG,
                                             dist_m = bufferDistance,
                                             output = 'sf')
        # rasterizing and making it into a mask
        buffer_rs <- fasterize::fasterize(buffer, sdm)
        buffer_rs[!is.na(buffer_rs[])] <- 1
        buffer_rs <- buffer_rs * sdmMask

        gPoints <- sp::SpatialPoints(raster::rasterToPoints(buffer_rs))
        # extract values from ecoregions to points
        raster::crs(gPoints) <- raster::crs(ecoReg)
        ecoValsG <- sp::over(x = gPoints, y = ecoReg) %>%
          dplyr::distinct(ECO_NUM)%>%
          tidyr::drop_na(ECO_NUM) %>% #ECO_ID
          dplyr::filter(ECO_NUM > 0) #ECO_ID

        speciesOcc <- speciesOcc[which(!is.na(speciesOcc$latitude)),]

        # create point object from all the occurence data for the species
        sp::coordinates(speciesOcc) <- ~longitude+latitude
        sp::proj4string(speciesOcc) <- sp::CRS("+proj=longlat +datum=WGS84")
        raster::crs(speciesOcc) <- raster::crs(ecoReg)

        # number of ecoregions present in model
        ecoVal <- data.frame(sp::over(x = speciesOcc, y = ecoReg))%>%
            dplyr::select(ECO_NUM )%>% #ECO_ID
            dplyr::distinct() %>%
            tidyr::drop_na() %>%
            dplyr::filter(ECO_NUM > 0) # -9998 are lakes #ECO_ID != -9998

        #calculate ERS
        ers <- min(c(100, (nrow(ecoValsG)/nrow(ecoVal))*100))
        # assign values to df
        df$species[i] <- as.character(species_list[i])
        df$ERSex[i] <- ers
      }
  }
  return(df)

}
