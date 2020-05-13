#' @title Geographical representativeness score estimation (Ex-situ conservation)
#' @name GRSex
#' @description This function performs an estimation of the geographical representativeness score
#'  for ex-situ gap analysis (GRSex) using Ramirez-Villegas et al., (2010) methodology.
#'  GRS ex-situ score is calculated as:
#'
#' \deqn{GRSex = min(100,(Masked Area of Buffered G Occurrences / Total Area of Predicted Habitat)*100)}
#'
#' @param Occurrence_data A data frame object with the species name, geographical coordinates,
#' and type of records (G or H) for a given species
#' @param Species_list An species list to calculate the GRSex metrics.
#' @param Raster_list A list representing the species distribution models for the species list provided
#'  loaded in raster format. This list must match the same order of the species list.
#' @param Buffer_distance Geographical distance used to create circular buffers around germplasm.
#'  Default: 50000 that is 50 km around germplasm accessions (CA50)
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
#' ## Obtaining rasterList objet. ##
#' data(CucurbitaRasters)
#' CucurbitaRasters <- raster::unstack(CucurbitaRasters)
#' #Calculating GRSex value
#' GRSex_df <- GRSex(Species_list = Cucurbita_splist,
#'                     Occurrence_data = CucurbitaData,
#'                     Raster_list = CucurbitaRasters,
#'                     Buffer_distance = 50000)
#'
#' @references
#' Ramirez-Villegas, J., Khoury, C., Jarvis, A., Debouck, D. G., & Guarino, L. (2010).
#' A Gap Analysis Methodology for Collecting Crop Genepools: A Case Study with Phaseolus Beans.
#' PLOS ONE, 5(10), e13497. Retrieved from https://doi.org/10.1371/journal.pone.0013497
#'
#' Khoury, C. K., Amariles, D., Soto, J. S., Diaz, M. V., Sotelo, S., Sosa, C. C., … Jarvis, A. (2019).
#' Comprehensiveness of conservation of useful wild plants: An operational indicator for biodiversity
#' and sustainable development targets. Ecological Indicators. https://doi.org/10.1016/j.ecolind.2018.11.016
#'
#'
#' Hijmans, R.J. and Spooner, D.M. (2001). Geographic distribution of wild potato species.
#' Am. J. Bot., 88: 2101-2112. doi:10.2307/3558435
#'
#' @export
#' @importFrom sp coordinates proj4string SpatialPoints over CRS
#' @importFrom stats median
#' @importFrom fasterize fasterize


GRSex <- function(Occurrence_data, Species_list, Raster_list, Buffer_distance) {

  longitude <- NULL
  taxon <- NULL
  type <- NULL
  latitude <-NULL

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


  # suppressMessages(require(rgdal))
  # suppressMessages(require(raster))

  #importFrom("methods", "as")
  #importFrom("stats", "complete.cases", "filter", "median")
  #importFrom("utils", "data", "memory.limit", "read.csv", "write.csv")
  if(missing(Buffer_distance)){
    Buffer_distance <- 50000
  }
  # create a dataframe to hold the components
  df <- data.frame(matrix(ncol = 2, nrow = length(Species_list)))
  colnames(df) <- c("species", "GRSex")

  for(i in seq_len(length(sort(Species_list)))){
    # select species G occurrences

    OccData  <- Occurrence_data[which(Occurrence_data$taxon==Species_list[i]),]
    OccData  <- OccData [which(OccData $type == "G" & !is.na(OccData $latitude)),]
    OccData  <- OccData [,c("longitude","latitude")]
    # OccData  <- Occurrence_data %>%
    #   tidyr::drop_na(longitude)%>%
    #   dplyr::filter(taxon == Species_list[i]) %>%
    #   dplyr::filter(type == "G")%>%
    #   dplyr::select(longitude,latitude)

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
#    buffer <- geobuffer::geobuffer_pts(xy = OccData ,
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
  }

  return(df)
}
