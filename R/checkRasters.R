#' @title Quality check on sdm imagery
#' @name checksdm
#' @description
#' Evaluates the class, crs, and values are standardizes to what the following gap analysis functions are required.
#'
#' @param sdm a terra rast object that represented the expected distribution of the species
#'
#' @return sdm : a terra rast object that is in the correct CRS
#'
#' @examples
#'#' @examples
#' ##Obtaining Raster_list
#' load("data/CucurbitaRasts.rda")
#'
#' # convert the dataset for function
#' sdm <- terra::unwrap(CucurbitaRasts)$cordata
#' #Running checksdm
#' sdm <- checksdm(sdm = sdm)
#'
#' @references
#' Khoury et al. (2019) Ecological Indicators 98:420-429. doi: 10.1016/j.ecolind.2018.11.016
#' Carver et al. (2021) GapAnalysis: an R package to calculate conservation indicators using spatial information
checksdm <- function(sdm){
  # check class and convert if needed
  c1 <- class(sdm)
  if(c1[1]!="Spatsdm"){
    sdm <- terra::rast(sdm)
    message(paste("Changed the object type from ", c1,
                  " to the required object terra rast"  ))
  }
  # check to see if this there is only one values in sdm
  vals <- unique(terra::values(sdm))[,1]
  vals <- vals[!is.na(vals)]
  if(length(vals) > 1 ){
    message(paste("Your input sdm contain the following values ", vals,
                  " please alter the sdm so the that only NA/NaN and 1 are present."  ))
    stop()
  }
  crs <- terra::crs(sdm)
  # crs
  if(!terra::same.crs(crs, terra::crs("epsg:4326"))){
    sdm <- terra::project(x = sdm, y = "epsg:4326" )
    message(paste("Changed the crs from ", crs,
                  " to the required epsg:4326"  ))
  }
  message(paste("All checks completed"))

  return(sdm)
}
