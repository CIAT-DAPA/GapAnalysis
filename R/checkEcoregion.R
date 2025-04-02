# test ecoregion files
##



# # custom input data  ------------------------------------------------------
sdm <- terra::rast("testData/Vitis acerifolia/prj_threshold.tif")
sdm <- subst(sdm, 0, NaN)
eco <- terra::vect("testData/us_eco_l3.shp")
uniqueID <- "US_L3CODE"
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

  return(ecos)
}
