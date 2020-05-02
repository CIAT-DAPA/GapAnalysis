suppressWarnings(suppressMessages(library(raster)))
lfiles <-list.files("data-raw/exampleData/sdm_rasters",full.names = T)
sdm_rasters <- lapply(1:length(lfiles), function(i){
  x <- raster::raster(lfiles[[i]])
  return(x)
})
usethis::use_data(sdm_rasters, overwrite = TRUE,compress = "bzip2")
