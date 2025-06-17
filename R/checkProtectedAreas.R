
#' @title Quality check on protected areas dataset
#' @name checkProtectedAreas
#' @description
#' Checks the class, values, crs, and cell size of the protected areas raster to ensure
#' these elements match those required by gap analysis functions.
#'
#' @param protectedAreas A terra rast object the contian spatial location of protected areas.
#' @param sdm a terra rast object that represented the expected distribution of the species
#'
#' @return protectedAreas : a terra rast object representing protected areas
#'
#' @examples
#' ##Obtaining Raster_list
#' load("data/CucurbitaRasts.rda")
#' ##Obtaining protected areas raster
#' load("data/protectAreasRast.rda")
#'
#' sdm <- terra::unwrap(CucurbitaRasts)$cordata
#' protectedAreas <- terra::unwrap(protectArea)
#'
#' #Running checkProtectedAreas
#' protectedAreas <- checkProtectedAreas(protectedAreas = protectedAreas,
#'                     sdm = sdm)
#'
#' @references
#' Khoury et al. (2019) Ecological Indicators 98:420-429. doi: 10.1016/j.ecolind.2018.11.016
#' Carver et al. (2021) GapAnalysis: an R package to calculate conservation indicators using spatial information
checkProtectedAreas <- function(protectedAreas, sdm){
  # check class and convert if needed
  c1 <- class(protectedAreas)
  if(c1[1] !="SpatRaster"){
    protectedAreas <- terra::rast(protectedAreas)
    message(paste("Changed the object type from ", c1,
                  " to the required object terra rast"  ))
  }
  # check to see if this there is only one values in raster
  vals <- unique(terra::values(protectedAreas))[,1]
  if(length(vals) > 2){
    message(paste("Your input raster contain the following values ", vals,
                  " please alter the raster so the that only NA/NaN and 1 are present."  ))
    stop()
  }

  crs <- terra::crs(protectedAreas)
  # crs
  if(!terra::same.crs(crs, terra::crs("epsg:4326"))){
    protectedAreas <- terra::project(x = protectedAreas, y = "epsg:4326" )
    message(paste("Changed the crs from ", crs,
                  " to the required epsg:4326"  ))
  }

  # cell size
  cellSize <- terra::res(protectedAreas)
  modelSize <- terra::res(sdm)
  if(cellSize[1] != modelSize[1] | cellSize[2] != modelSize[2] ){
    protectedAreas <- resample(protectedAreas, sdm, method = "near")
    # this statement is printing twice? not sure why
    message(paste("Changed the cell size of the protected area from ", cellSize,
                  " to the cell size of the sdm ", modelSize  ))
  }
  message(paste("All checks completed"))

  return(protectedAreas)
}
