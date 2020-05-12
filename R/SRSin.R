#' @title Sample representativeness score estimation (In-situ conservation)
#' @name SRSin
#' @description This function performs an estimation of the sample representativeness
#'  score for in situ gap analysis (SRSin) using Khoury et al., (2019) methodology.
#'  SRSin is calculated as:
#'  \deqn{SRSin = Number of occurrences in protected areas / Total number of occurrences}
#'
#' @param species_list An species list to calculate the SRSin metrics.
#' @param occurrenceData A data frame object with the species name, geographical coordinates,
#'  and type of records (G or H) for a given species
#' @param raster_list A list representing the species distribution models for the species list
#'  provided loaded in raster format. This list must match the same order of the species list.
#' @param proArea A raster file representing protected areas information.
#'  If proArea=NULL the function will use a protected area raster file
#'  provided for your use after run GetDatasets()
#' @return This function returns a data frame with two columns:
#'
#' \tabular{lcc}{
#' species \tab Species name \cr
#' SRSin \tab srsInsitu value calculated\cr
#' }
#'
#' @examples
#' ##Obtaining occurrences from example
#' data(CucurbitaData)
#' ##Obtaining species names from the data
#' speciesList <- unique(CucurbitaData$taxon)
#' ##Obtaining raster_list
#' data(CucurbitaRasters)
#' CucurbitaRasters <- raster::unstack(CucurbitaRasters)
#' ##Obtaining protected areas raster
#' data(ProtectedAreas)
#' SRSin_df <- SRSin(species_list = speciesList,
#'                     occurrenceData = CucurbitaData,
#'                     raster_list=CucurbitaRasters,
#'                     proArea=ProtectedAreas)
#'
#'@references
#'
#' Ramirez-Villegas, J., Khoury, C., Jarvis, A., Debouck, D. G., & Guarino, L. (2010).
#' A Gap Analysis Methodology for Collecting Crop Genepools: A Case Study with Phaseolus Beans.
#' PLOS ONE, 5(10), e13497. Retrieved from https://doi.org/10.1371/journal.pone.0013497
#'
#' Khoury, C. K., Amariles, D., Soto, J. S., Diaz, M. V., Sotelo, S., Sosa, C. C., … Jarvis, A. (2019).
#' Comprehensiveness of conservation of useful wild plants: An operational indicator for biodiversity
#' and sustainable development targets. Ecological Indicators. https://doi.org/10.1016/j.ecolind.2018.11.016
#'
#' @export
#' @importFrom raster raster crop


SRSin <- function(species_list, occurrenceData, raster_list,proArea){

  taxon <- NULL
  longitude <- NULL
  #importFrom("methods", "as")
  #importFrom("stats", "complete.cases", "filter", "median")
  #importFrom("utils", "data", "memory.limit", "read.csv", "write.csv")

  #Checking occurrenceData format
  par_names <- c("taxon","latitude","longitude","type")

  if(identical(names(occurrenceData),par_names)==FALSE){
    stop("Please format the column names in your dataframe as taxon,latitude,longitude,type")
  }
  #Checking if user is using a raster list or a raster stack
  if(class(raster_list)=="RasterStack"){
    raster_list <- raster::unstack(raster_list)
  } else {
    raster_list <- raster_list
  }

  # Load in protect areas

  if(is.null(proArea)){
    if(file.exists(system.file("data/preloaded_data/protectedArea/wdpa_reclass.tif",package = "GapAnalysis"))){
      proArea <- raster::raster(system.file("data/preloaded_data/protectedArea/wdpa_reclass.tif",package = "GapAnalysis"))
    } else {
      stop("Protected areas file is not available yet. Please run the function preparingDatasets()  and try again")
    }
  } else{
    proArea <- proArea
  }

  # create an empty dataframe
  df <- data.frame(matrix(ncol = 2, nrow = length(species_list)))
  colnames(df) <- c("species", "SRSin")

  for(i in seq_len(length(species_list))){
    # pull the sdm to mask for
    for(j in seq_len(length(raster_list))){
      if(grepl(j, i, ignore.case = TRUE)){
        sdm <- raster_list[[j]]
      }
    };rm(j)
    # restrict protect areas those that are present in the model threshold
    ##**double check about this step with jullian/chrys/colin**
    proArea1 <- raster::crop(x = proArea,y = sdm)
    sdm[sdm == 0]<-NA
    proAreaSpecies <- sdm * proArea1

    # filter by specific species

    occData1 <- occurrenceData[which(occurrenceData$taxon==species_list[i] & !is.na(occurrenceData$latitude)),]
    # occData1 <- occurrenceData %>%
    #     dplyr::filter(taxon == species_list[i])%>%
    #       tidyr::drop_na(longitude)
     totalNum <- nrow(occData1)

    # extract values to all points
    sp::coordinates(occData1) <- ~longitude+latitude
    sp::proj4string(occData1) <- sp::CRS("+proj=longlat +datum=WGS84")
    protectPoints <- sum(!is.na(raster::extract(x = proArea1,y = occData1)))

    #define SRS
    if(protectPoints >= 0 ){
      srsInsitu <- 100 *(protectPoints/totalNum)
    }else{
      srsInsitu <- 0
    }
  # add values to empty df
    df$species[i] <- as.character(species_list[i])
    df$SRSin[i] <- srsInsitu
  };rm(i)
return(df)
}
