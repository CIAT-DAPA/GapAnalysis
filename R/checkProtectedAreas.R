
# # custom input data  ------------------------------------------------------
# sdm <- terra::rast("testData/Vitis acerifolia/prj_threshold.tif")
# sdm <- subst(sdm, 0, NaN)
# proArea <- terra::rast("testData/wdpa_reclass.tif")

checkProtectAreas <- function(proArea, sdm){
  # check class and convert if needed
  c1 <- class(proArea)
  if(c1[1] !="SpatRaster"){
    proArea <- terra::rast(proArea)
    message(paste("Changed the object type from ", c1,
                  " to the required object terra rast"  ))
  }
  # check to see if this there is only one values in raster
  vals <- unique(terra::values(proArea))[,1]
  if(length(vals) > 2){
    message(paste("Your input raster contain the following values ", vals,
                  " please alter the raster so the that only NA/NaN and 1 are present."  ))
    stop()
  }

  crs <- terra::crs(proArea)
  # crs
  if(!terra::same.crs(crs, terra::crs("epsg:4326"))){
    proArea <- terra::project(x = proArea, y = "epsg:4326" )
    message(paste("Changed the crs from ", crs,
                  " to the required epsg:4326"  ))
  }

  # cell size
  cellSize <- terra::res(proArea)
  modelSize <- terra::res(sdm)
  if(cellSize[1] != modelSize[1] | cellSize[2] != modelSize[2] ){
    proArea <- resample(proArea, sdm, method = "near")
    # this statement is printing twice? not sure why
    message(paste("Changed the cell size of the protected area from ", cellSize,
                  " to the cell size of the sdm ", modelSize  ))
  }
  message(paste("All checks completed"))

  return(proArea)
}
