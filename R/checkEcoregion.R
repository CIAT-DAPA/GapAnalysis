#' @param eco
#'
#' @param sdm
#' @param uniqueID
#'
#' @title Download datasets from Dataverse
#' @name GetDataSets
#' @description
#' A short description...
#' @param eco
#'
#' @param sdm
#' @param uniqueID
#'
#' @return
#'
#' @examples
#'
#'
#' @references
#' Khoury et al. (2019) Ecological Indicators 98:420-429. doi: 10.1016/j.ecolind.2018.11.016
#' Carver et al. (2021) GapAnalysis: an R package to calculate conservation indicators using spatial information
checkEcoregion <- function(eco, sdm, uniqueID){
  # check class and convert if needed
  c1 <- class(eco)
  if(c1[1] !="SpatVector"){
    eco <- terra::vect(eco)
    message(paste("Changed the object type from ", c1,
                  " to the required object terra vector"  ))
  }

  crs <- terra::crs(ecos)
  # crs
  if(!terra::same.crs(crs, terra::crs("epsg:4326"))){
    ecos<- terra::project(x = ecos, y = "epsg:4326" )
    message(paste("Changed the crs from ", crs,
                  " to the required epsg:4326"  ))
  }

  # test the unique ID column
  rows <- nrow(eco)
  unID <- length(eco[,uniqueID])
  if(rows != unID){
    message(paste("The total rows of your ecoregion object is", rows,
                  " the unique values fo the ", uniqueID, " column is ", unID,
                  "these should match. Please try a new column or you may get some unexpected results"))
  }
  # extent
  ## not going to worry about the extent here
  ## basically it can be limited by either the points, the buffered points, or
  ## the sdm. Because of that variabily it's better to handle this in the specific
  ## functions
  message(paste("All checks completed"))

  return(ecos)
}
