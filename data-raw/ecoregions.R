suppressWarnings(suppressMessages(library(raster)))
#suppressWarnings(require(magrittr))
suppressWarnings(suppressMessages(require(dplyr)))
ecoregions <- raster::shapefile("data-raw/fileData/ecoRegion/tnc_terr_ecoregions.shp")
ecoregions@data <-  ecoregions@data %>%
 dplyr::mutate_if(is.character, iconv, to = 'UTF-8')
# writeOGR(ecoregions, dsn = "E:/gapAnalysisR_DAN/GapAnalysisR_test-test_april/data-raw" ,
#          layer = "tnc_terr_ecoregions",driver="ESRI Shapefile")


usethis::use_data(ecoregions, overwrite = TRUE,compress = "bzip2")
