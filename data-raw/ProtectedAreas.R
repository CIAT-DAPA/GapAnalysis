suppressWarnings(suppressMessages(library(raster)))
ProtectedAreas <- raster::raster("data-raw/fileData/ProtectedArea/wdpa_reclass.tif")
ProtectedAreas <- readAll(ProtectedAreas)
usethis::use_data(ProtectedAreas, overwrite = TRUE,compress = "bzip2")

