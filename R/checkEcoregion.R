#' @title Quality check of ecoregion dataset
#' @name checkEcoregion
#' @description
#' Checks the class, crs, if the idColumn is a unique ID,
#' @param ecoregion A terra vect object the contains spatial information on all ecoregions of interests
#' @param sdm a terra rast object that represented the expected distribution of the species
#' @param idColumn A character vector that notes what column within the ecoregions object should be used as a unique ID
#'
#' @return ecoregions : A terra vect object the contains spatial information on all ecoregions of interests
#'
#' @examples
#' ##Obtaining Raster_list
# data(CucurbitaRasts)
# ## ecoregion features
# data(ecoregions)
#
# # convert the dataset for function
# sdm <- terra::unwrap(CucurbitaRasts)$cordata
# ecoregions <- terra::vect(ecoregions)
# #Running checkEcoregion
# ecoregions <- checkEcoregion(ecoregion = ecoregions,
#                     sdm = sdm,
#                     idColumn = "ECO_NAME"
#                     )
#'
#' @references
#' Khoury et al. (2019) Ecological Indicators 98:420-429. doi: 10.1016/j.ecolind.2018.11.016
#' Carver et al. (2021) GapAnalysis: an R package to calculate conservation indicators using spatial information
#' @importFrom terra vect crs project same.crs
#' @export

checkEcoregion <- function(ecoregion, sdm, idColumn){
  # check class and convert if needed
  c1 <- class(ecoregion)
  if(c1[1] !="SpatVector"){
    ecoregion <- terra::vect(ecoregion)
    message(paste("Changed the object type from ", c1,
                  " to the required object terra vector"  ))
  }

  crs <- terra::crs(ecoregion)
  # crs
  if(!terra::same.crs(crs, terra::crs("epsg:4326"))){
    ecoregion<- terra::project(x = ecoregion, y = "epsg:4326" )
    message(paste("Changed the crs from ", crs,
                  " to the required epsg:4326"  ))
  }

  # test the unique ID column
  rows <- nrow(ecoregion)
  unID <- length(ecoregion[[idColumn]][,1])
  if(rows != unID){
    message(paste("The total rows of your ecoregion object is", rows,
                  " the unique values fo the ", idColumn, " column is ", unID,
                  "these should match. Please try a new column or you may get some unexpected results"))
  }

  message(paste("All checks completed"))

  return(ecoregion)
}
