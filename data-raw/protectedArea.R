suppressWarnings(suppressMessages(library(raster)))
protectedArea <- raster::raster("data-raw/fileData/protectedArea//wdpa_reclass.tif")
usethis::use_data(protectedArea, overwrite = TRUE,compress = "bzip2")

