suppressWarnings(suppressMessages(library(raster)))
#suppressWarnings(require(magrittr))
suppressWarnings(suppressMessages(require(dplyr)))
Ecoregions  <- raster::shapefile("data-raw/fileData/Ecoregions/tnc_terr_ecoregions.shp")
Ecoregions@data <-  Ecoregions@data %>%
 dplyr::mutate_if(is.character, iconv, to = 'UTF-8')

usethis::use_data(Ecoregions, overwrite = TRUE,compress = "bzip2")
