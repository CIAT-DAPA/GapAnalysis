#' @title Sample representativeness score estimation (In-situ conservation)
#' @name SRSin
#' @description This function performs an estimation of the sample representativeness
#'  score for in situ gap analysis (SRSin) using Khoury et al., (2019) methodology.
#'  SRSin is calculated as:
#'  \deqn{SRSin = Number of occurrences in protected areas / Total number of occurrences}
#'
#' @param Species_list An species list to calculate the SRSin metrics.
#' @param Occurrence_data A data frame object with the species name, geographical coordinates,
#'  and type of records (G or H) for a given species
#' @param Raster_list A list representing the species distribution models for the species list
#'  provided loaded in raster format. This list must match the same order of the species list.
#' @param Pro_areas A raster file representing protected areas information.
#'  If Pro_areas=NULL the function will use a protected area raster file
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
#' Cucurbita_splist <- unique(CucurbitaData$taxon)
#' ##Obtaining Raster_list
#' data(CucurbitaRasters)
#' CucurbitaRasters <- raster::unstack(CucurbitaRasters)
#' ##Obtaining protected areas raster
#' data(ProtectedAreas)
#' SRSin_df <- SRSin(Species_list = Cucurbita_splist,
#'                     Occurrence_data = CucurbitaData,
#'                     Raster_list=CucurbitaRasters,
#'                     Pro_areas=ProtectedAreas)
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


SRSin <- function(Species_list, Occurrence_data, Raster_list,Pro_areas){

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

  # Load in protect areas

  if(is.null(Pro_areas)){
    if(file.exists(system.file("data/preloaded_data/protectedArea/wdpa_reclass.tif",package = "GapAnalysis"))){
      Pro_areas <- raster::raster(system.file("data/preloaded_data/protectedArea/wdpa_reclass.tif",package = "GapAnalysis"))
    } else {
      stop("Protected areas file is not available yet. Please run the function GetDatasets()  and try again")
    }
  } else{
    Pro_areas <- Pro_areas
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
    # restrict protect areas those that are present in the model threshold
    ##**double check about this step with jullian/chrys/colin**
    Pro_areas1 <- raster::crop(x = Pro_areas,y = sdm)
    sdm[sdm == 0]<-NA
    Pro_areasSpecies <- sdm * Pro_areas1

    # filter by specific species

    occData1 <- Occurrence_data[which(Occurrence_data$taxon==Species_list[i] & !is.na(Occurrence_data$latitude)),]
    # occData1 <- Occurrence_data %>%
    #     dplyr::filter(taxon == Species_list[i])%>%
    #       tidyr::drop_na(longitude)
     totalNum <- nrow(occData1)

    # extract values to all points
    sp::coordinates(occData1) <- ~longitude+latitude
    sp::proj4string(occData1) <- sp::CRS("+proj=longlat +datum=WGS84")
    protectPoints <- sum(!is.na(raster::extract(x = Pro_areas1,y = occData1)))

    #define SRS
    if(protectPoints >= 0 ){
      srsInsitu <- 100 *(protectPoints/totalNum)
    }else{
      srsInsitu <- 0
    }
  # add values to empty df
    df$species[i] <- as.character(Species_list[i])
    df$SRSin[i] <- srsInsitu
  };rm(i)
return(df)
}
