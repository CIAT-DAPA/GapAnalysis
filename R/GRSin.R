#' @title Geographical representativeness score estimation (In-situ conservation)
#' @name GRSin
#' @description This function performs an estimation of germplasm representativeness score for in-situ gap analysis (GRSin) using Khoury et al., (2019) methodology
#'#'  GRSIn is calculated as:
#'  \deqn{GRSin = min(100,(Predicted Habitat within protected areas/ Total Area of Predicted Habitat)*100)}
#'
#' @param species_list An species list to calculate the GRSin metrics.
#' @param occurrenceData A data frame object with the species name, geographical coordinates, and type of records (G or H) for a given species
#' @param raster_list A list representing the species distribution models for the species list provided loaded in raster format. This list must match the same order of the species list.
#' @param proArea A raster file representing protected areas information. If proArea=NULL the function will use a protected area raster file
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
#' speciesList <- unique(CucurbitaData$taxon)
#' ##Obtaining raster_list
#' data(CucurbitaRasters)
#' ##Obtaining protected areas raster
#' data(ProtectedAreas)
#'
#' GRSin_df <- GRSin(species_list = speciesList,
#'                     occurrenceData = CucurbitaData,
#'                     raster_list = CucurbitaRasters,
#'                     proArea=ProtectedAreas)
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
#' @importFrom dplyr filter
#' @importFrom tidyr drop_na


GRSin = function(species_list,occurrenceData,raster_list,proArea){

# suppressMessages(require(rgdal))
# suppressMessages(require(raster))
# suppressMessages(require(tmap))
# suppressMessages(require(fasterize))
# suppressMessages(require(sf))

#importFrom("methods", "as")
#importFrom("stats", "complete.cases", "filter", "median")
#importFrom("utils", "data", "memory.limit", "read.csv", "write.csv")
  df <- data.frame(matrix(ncol=2, nrow = length(species_list)))
  colnames(df) <- c("species", "GRSin")
  # load in protect area raster
  if(is.null(proArea)){
    if(file.exists(system.file("data/preloaded_data/protectedArea/wdpa_reclass.tif",package = "GapAnalysis"))){
      proArea <- raster::raster(system.file("data/preloaded_data/protectedArea/wdpa_reclass.tif",package = "GapAnalysis"))
    } else {
      stop("Protected areas file is not available yet. Please run the function preparingDatasets()  and try again")
    }
  } else{
    proArea = proArea
  }

  # loop over species list
  for(i in 1:length(species_list)){
    # select threshold map for a given species
    for(j in 1:length(raster_list)){
      if(grepl(j, i, ignore.case = TRUE)){
        sdm <- raster_list[[j]]
      }
    }
    # determine the area of predicted presence of a species based on the threshold map
    sdm1 <- sdm
    proArea1 <- raster::crop(x = proArea,y = sdm1)
    sdm1[sdm1 == 0] <- NA
    cell_size <- raster::area(sdm1, na.rm=TRUE, weights=FALSE)
    cell_size <- cell_size[!is.na(cell_size)]
    thrshold_area <- length(cell_size)*median(cell_size)

    # mask the protected area Raster to the threshold map and calculate area
    proArea1[proArea1 == 0] <-NA
    proArea1 <- proArea1 * sdm1
    # calculate area
    cell_size <- raster::area(proArea1, na.rm=TRUE, weights=FALSE)
    cell_size <- cell_size[!is.na(cell_size)]
    protected_area <- length(cell_size)*stats::median(cell_size)
    if(!is.na(protected_area)){
      # calculate GRSin
      grs <- min(c(100, protected_area/thrshold_area*100))
      df$species[i] <- as.character(species_list[i])
      df$GRSin[i] <- grs
    }else{
      df$species[i] <- as.character(species_list[i])
      df$GRSin[i] <- 0
    }
  }
  return(df)
}
