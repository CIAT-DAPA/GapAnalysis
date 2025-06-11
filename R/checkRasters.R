
#' Title
#'
#' @param raster
#'
#' @return
#' @export
#'
#' @examples
checkRaster <- function(raster){
  # check class and convert if needed
  c1 <- class(raster)
  if(c1[1]!="SpatRaster"){
    raster <- terra::rast(raster)
    message(paste("Changed the object type from ", c1,
                  " to the required object terra rast"  ))
  }
  # check to see if this there is only one values in raster
  vals <- unique(terra::values(raster))[,1]
  vals <- vals[!is.na(vals)]
  if(length(vals) > 1 ){
    message(paste("Your input raster contain the following values ", vals,
                  " please alter the raster so the that only NA/NaN and 1 are present."  ))
    stop()
  }
  crs <- terra::crs(raster)
  # crs
  if(!terra::same.crs(crs, terra::crs("epsg:4326"))){
    raster <- terra::project(x = raster, y = "epsg:4326" )
    message(paste("Changed the crs from ", crs,
                  " to the required epsg:4326"  ))
  }
  message(paste("All checks completed"))

  return(raster)
}
