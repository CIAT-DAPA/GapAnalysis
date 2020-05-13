#' @title Geographical representativeness score estimation (In-situ conservation)
#' @name GRSin
#' @description This function performs an estimation of germplasm representativeness score for in-situ gap analysis (GRSin) using Khoury et al., (2019) methodology
#'#'  GRSIn is calculated as:
#'  \deqn{GRSin = min(100,(Predicted Habitat within protected areas/ Total Area of Predicted Habitat)*100)}
#'
#' @param Species_list An species list to calculate the GRSin metrics.
#' @param Occurrence_data A data frame object with the species name, geographical coordinates, and type of records (G or H) for a given species
#' @param Raster_list A list representing the species distribution models for the species list provided loaded in raster format. This list must match the same order of the species list.
#' @param Pro_areas A raster file representing protected areas information. If Pro_areas=NULL the function will use a protected area raster file
#'  provided for your use after run GetDatasets()
#'
#' @return This function returns a data frame with two columns:
#'
#' \tabular{lcc}{
#' species \tab Species name \cr
#' GRSin \tab GRSin value calculated\cr
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
#'
#' GRSin_df <- GRSin(Species_list = Cucurbita_splist,
#'                     Occurrence_data = CucurbitaData,
#'                     Raster_list = CucurbitaRasters,
#'                     Pro_areas=ProtectedAreas)
#'
#'
#'@references
#' Ramirez-Villegas, J., Khoury, C., Jarvis, A., Debouck, D. G., & Guarino, L. (2010).
#' A Gap Analysis Methodology for Collecting Crop Genepools: A Case Study with Phaseolus Beans.
#' PLOS ONE, 5(10), e13497. Retrieved from https://doi.org/10.1371/journal.pone.0013497
#'
#' Khoury, C. K., Amariles, D., Soto, J. S., Diaz, M. V., Sotelo, S., Sosa, C. C., … Jarvis, A. (2019).
#' Comprehensiveness of conservation of useful wild plants: An operational indicator for biodiversity
#' and sustainable development targets. Ecological Indicators. https://doi.org/10.1016/j.ecolind.2018.11.016
#'
#' @export
#' @importFrom stats median
#' @importFrom raster raster crop area



GRSin <- function(Species_list,Occurrence_data,Raster_list,Pro_areas){

# suppressMessages(require(rgdal))
# suppressMessages(require(raster))
# suppressMessages(require(tmap))
# suppressMessages(require(fasterize))
# suppressMessages(require(sf))

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


  df <- data.frame(matrix(ncol=2, nrow = length(Species_list)))
  colnames(df) <- c("species", "GRSin")
  # load in protect area raster
  if(is.null(Pro_areas)){
    if(file.exists(system.file("data/preloaded_data/protectedArea/wdpa_reclass.tif",package = "GapAnalysis"))){
      Pro_areas <- raster::raster(system.file("data/preloaded_data/protectedArea/wdpa_reclass.tif",package = "GapAnalysis"))
    } else {
      stop("Protected areas file is not available yet. Please run the function GetDatasets()  and try again")
    }
  } else{
    Pro_areas <- Pro_areas
  }

  # loop over species list
  for(i in seq_len(length(Species_list))){
    # select threshold map for a given species
    for(j in seq_len(length(Raster_list))){
      if(grepl(j, i, ignore.case = TRUE)){
        sdm <- Raster_list[[j]]
      }
    }
    # determine the area of predicted presence of a species based on the threshold map
    sdm1 <- sdm
    Pro_areas1 <- raster::crop(x = Pro_areas,y = sdm1)
    sdm1[sdm1[] == 0] <- NA
    cell_size <- raster::area(sdm1, na.rm=TRUE, weights=FALSE)
    cell_size <- cell_size[!is.na(cell_size)]
    thrshold_area <- length(cell_size)*median(cell_size)

    # mask the protected area Raster to the threshold map and calculate area
    Pro_areas1[Pro_areas1[] == 0] <-NA
    Pro_areas1 <- Pro_areas1 * sdm1
    # calculate area
    cell_size <- raster::area(Pro_areas1, na.rm=TRUE, weights=FALSE)
    cell_size <- cell_size[!is.na(cell_size)]
    protected_area <- length(cell_size)*stats::median(cell_size)
    if(!is.na(protected_area)){
      # calculate GRSin
      GRSin <- min(c(100, protected_area/thrshold_area*100))
      df$species[i] <- as.character(Species_list[i])
      df$GRSin[i] <- GRSin
    }else{
      df$species[i] <- as.character(Species_list[i])
      df$GRSin[i] <- 0
    }
  }
  return(df)
}
