suppressWarnings(suppressMessages(library(raster)))
#suppressWarnings(require(magrittr))
suppressWarnings(suppressMessages(require(dplyr)))
ecoregions  <- raster::shapefile("data-raw/fileData/ecoregions/tnc_terr_ecoregions.shp")
ecoregions@data <-  ecoregions@data %>%
 dplyr::mutate_if(is.character, iconv, to = 'UTF-8')

usethis::use_data(ecoregions, overwrite = TRUE,compress = "bzip2")
