suppressWarnings(suppressMessages(library(raster)))
setwd("~/Repositories/GapAnalysis")
lfiles <-list.files("data-raw/fileData/cucurbitaRasters",full.names = T,pattern = ".tif")
cucurbitaRasters <- lapply(1:length(lfiles), function(i){
  x <- raster::raster(lfiles[[i]])
  return(x)
})

names(cucurbitaRasters[[1]]) <- "cordata"
names(cucurbitaRasters[[2]]) <- "digitata"
names(cucurbitaRasters[[3]]) <- "palmata"

cucurbitaRasters <- stack(cucurbitaRasters)
cucurbitaRasters <- readAll(cucurbitaRasters)
usethis::use_data(cucurbitaRasters, overwrite = TRUE,compress = "bzip2")
