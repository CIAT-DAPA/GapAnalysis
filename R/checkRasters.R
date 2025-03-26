# load("data/CucurbitaRasts.rda")
#
#
# rasters <- terra::unwrap(CucurbitaRasts)
#
# raster <- rasters$cordata
#
# checkRaster(raster)

checkRaster <- function(raster){
  # check class and convert if needed
  c1 <- class(raster)
  if(class(c1)!="SpatRaster"){
    raster <- terra::rast(raster)
    message(paste("Changed the object type from ", c1,
                  " to the required object terra rast"  ))
  }

  crs <- terra::crs(raster)
  # crs
  if(!terra::same.crs(crs, terra::crs("epsg:4326"))){
    raster <- terra::project(x = raster, y = "epsg:4326" )
    message(paste("Changed the crs from ", crs,
                  " to the required epsg:4326"  ))
  }

  return(raster)
}
