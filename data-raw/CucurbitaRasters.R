suppressWarnings(suppressMessages(library(raster)))
lfiles <-list.files("data-raw/fileData/CucurbitaRasters",full.names = T,pattern = ".tif")
CucurbitaRasters <- lapply(1:length(lfiles), function(i){
  x <- raster::raster(lfiles[[i]])
  return(x)
})

names(CucurbitaRasters[[1]]) <- "cordata"
names(CucurbitaRasters[[2]]) <- "digitata"
names(CucurbitaRasters[[3]]) <- "palmata"

CucurbitaRasters <- stack(CucurbitaRasters)
usethis::use_data(CucurbitaRasters, overwrite = TRUE,compress = "bzip2")
