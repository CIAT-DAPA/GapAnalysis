#
# ##Obtaining occurrences from example
# load("data/CucurbitaData.rda")
# ##Obtaining species names from the data
# taxon <- CucurbitaData$species[1]
# ##Obtaining Raster_list
# load("data/CucurbitaRasts.rda")
# ##Obtaining ecoregions layer
# load("data/ecoExample.rda")
#
#
# taxon <- taxon
# sdm <- terra::unwrap(CucurbitaRasts)$cordata
# occurrence_Data <- CucurbitaData
# ecoregions <- terra::vect(eco1)
# source("R/generateGBuffers.R")
# gBuffer <- generateGBuffers(taxon = taxon,
#                             occurrence_Data = occurrence_Data,
#                             bufferDistM =  50000)
# idColumn  <- "ECO_ID_U"
#
# ERSex(taxon = taxon,
#       sdm = sdm,
#       occurrence_Data = occurrence_Data,
#       gBuffer = gBuffer,
#       ecoregions = ecoregions,
#       idColumn = "ECO_ID_U")

ERSex <- function(taxon, sdm, occurrence_Data, gBuffer, ecoregions, idColumn){
  # filter the occurrence data to the species of interest
  d1 <- occurrence_Data |>
    dplyr::filter(species == taxon) |>
    terra::vect(geom=c("longitude", "latitude"))

  # determine the eco regions present in the
  inter <- terra::intersect(x = d1, y = ecoregions) |>
    terra::as.data.frame()
  # select ecoregions of interest
  ecoCodes <- unique(inter[,idColumn])
  # index with selection
  ## conver to table for easier indexing
  eco2 <- terra::as.data.frame(ecoregions)
  ## select
  n1 <- ecoregions[eco2[,idColumn] %in% ecoCodes, ]

  # get a count in each ecoregion
  v1 <- terra::zonal(x = sdm,
                     z = n1,
                     fun="sum",
                     na.rm=TRUE)
  v1$ECO_ID_U <- n1$ECO_ID_U

  # assign names for better indexing
  names(v1) <- c("count","ecoID")
  # sum up all features based on eco ID
  v2 <- v1 |>
    dplyr::group_by(ecoID)|>
    dplyr::summarise(
      cellsInEco = sum(count, na.rm=TRUE)
    )


  # Number of ecoregions considered.
  nEco <- v2 |>
    dplyr::filter(cellsInEco > 0) |>
    nrow()


  if(class(gBuffer) == "Character"){
    ers <- 0
    gEco <- NA
    missingEcos <- v1$ECO_ID_U
  }else{
    # rasterize the buffer object
    b1 <- terra::rasterize(x = gBuffer, y = sdm)
    # determine ecoregions in ga50 area
    v2 <- terra::zonal(x = b1,z = n1,fun="sum",na.rm=TRUE)
    v2$ECO_ID_U <- n1$ECO_ID_U
    #assing names for easier indexing
    names(v2) <- c("layer", "ecoID")
    # determine the ecoregions that are not being considered
    areasWithGBuffer <- v2 |>
      filter(layer >0) |>
      filter(!is.nan(layer))
    # get the total number number of eco regions with a g buffer area
    gEco <- areasWithGBuffer |>
      dplyr::select(ecoID)|>
      distinct()|>
      nrow()
    # generate a list of the ecoregions ID that are inside the threshold but have no g buffer
    missingEcos <- v1 |>
      dplyr::filter(count >0) |>
      dplyr::filter(!ecoID %in% areasWithGBuffer$ecoID)|>
      dplyr::select(ecoID)|>
      distinct()|>
      pull()
  }

  # ERs calculation
  ers <- min(c(100, (gEco/nEco)*100))
  # generate filter

  out_df = data.frame(Taxon=taxon,
                      `Ecoregions with records` =nEco,
                      `Ecoregions within G buffer` =gEco,
                      `ERS exsitu` =ers)
  out_df$`Ecoregion id outside G buffer` <- list(missingEcos)

  # generate dataframe
  return(out_df)
}

