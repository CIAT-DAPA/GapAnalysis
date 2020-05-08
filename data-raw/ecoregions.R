suppressWarnings(suppressMessages(library(raster)))
#suppressWarnings(require(magrittr))
suppressWarnings(suppressMessages(require(dplyr)))
ecoregions  <- raster::shapefile("data-raw/fileData/ecoregions/tnc_terr_ecoregions.shp", encoding = "UTF-8")

nc <- dim(ecoregions@data)[[2]]
ecoregions@data[,1:nc] <-lapply(1:nc, function(x){
  if (is.character(ecoregions@data[,x]) | is.factor(ecoregions@data[,x])) {
    iconv(ecoregions@data[,x], to = "UTF-8")
  } else {
    ecoregions@data[,x] <- ecoregions@data[,x]
  }
})

# ecoregions@data <-  ecoregions@data %>%
#  dplyr::mutate_if(is.character, iconv, to = 'UTF-8')
usethis::use_data(ecoregions, overwrite = TRUE,compress = "bzip2")
