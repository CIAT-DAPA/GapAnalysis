suppressWarnings(suppressMessages(library(raster)))
ProtectedAreas <- raster::raster("data-raw/fileData/protectedArea/wdpa_reclass.tif")
usethis::use_data(ProtectedAreas, overwrite = TRUE,compress = "bzip2")

