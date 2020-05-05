suppressWarnings(suppressMessages(library(raster)))
protectedAreas <- raster::raster("data-raw/fileData/protectedArea/wdpa_reclass.tif")
protectedAreas <- readAll(protectedAreas)
usethis::use_data(protectedAreas, overwrite = TRUE,compress = "bzip2")

