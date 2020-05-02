suppressWarnings(suppressMessages(library(raster)))
lfiles <-list.files("data-raw/fileData/CucurbitaRasters",full.names = T,pattern = ".tif")
sdm_rasters <- lapply(1:length(lfiles), function(i){
  x <- raster::raster(lfiles[[i]])
  return(x)
})

names(sdm_rasters[[1]]) <- "cordata"
names(sdm_rasters[[2]]) <- "digitata"
names(sdm_rasters[[3]]) <- "palmata"

sdm_rasters <- stack(sdm_rasters)
usethis::use_data(sdm_rasters, overwrite = TRUE,compress = "bzip2")
