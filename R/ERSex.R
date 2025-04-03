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
    terra::vect(geom=c("longitude", "latitude"), crs="+proj=longlat +datum=WGS84")
  # determine the eco regions present in the
  ecoregions$sdmSum <- terra::zonal(x = sdm, z = ecoregions, fun = "sum", na.rm=TRUE)
  # subset ecoregions to feautres with greated then 0
  ecoSelect <- ecoregions[ecoregions$sdmSum >0, ]
  # select ecoregions of interest
  ecoCodes <- unique(ecoSelect[,idColumn])
  # # index with selection
  # ## conver to table for easier indexing
  eco2 <- terra::as.data.frame(ecoSelect) |>
    dplyr::select(ecoID = idColumn, count = sdmSum)
  # ## select
  # n1 <- ecoregions[eco2[,idColumn] %in% ecoCodes, ]

  # # get a count in each ecoregion
  # v1 <- terra::zonal(x = sdm,
  #                    z = n1,
  #                    fun="sum",
  #                    na.rm=TRUE)
  # v1$ECO_ID_U <- n1$ECO_ID_U
  #
  # # assign names for better indexing
  # names(v1) <- c("","ecoID")
  # sum up all features based on eco ID
  # v2 <- eco2 |>
  #   dplyr::group_by(ecoID)|>
  #   dplyr::summarise(
  #     cellsInEco = sum(count, na.rm=TRUE)
  #   )


  # Number of ecoregions considered.
  nEco <- nrow(eco2)

  if(class(gBuffer) == "Character"){
    ers <- 0
    gEco <- NA
    missingEcos <- eco2$ecoID
  }else{
    # rasterize the buffer object
    b1 <- terra::rasterize(x = gBuffer, y = sdm) |> terra::mask(sdm)
    # determine ecoregions in ga50 area
    eco2$bufferEcos <- terra::zonal(x = b1,z = ecoSelect,fun="sum",na.rm=TRUE) |> unlist()
    #assing names for easier indexing
    names(eco2) <- c("ecoID ", "count", "bufferEcos")
    # determine the ecoregions that are not being considered
    areasWithOutGBuffer <- eco2 |>
      filter(is.nan(bufferEcos) | is.na(bufferEcos))
    # get the total number number of eco regions with a g buffer area
    gEco <- eco2 |>
      filter(bufferEcos >0 )|>
      filter(!is.nan(bufferEcos))|>
      nrow()
    # generate a list of the ecoregions ID that are inside the threshold but have no g buffer
    missingEcos <- nrow(areasWithOutGBuffer)
  }

  # ERs calculation
  ers <- min(c(100, (gEco/nEco)*100))
  # generate filter

  out_df = dplyr::tibble(Taxon=taxon,
                      `Ecoregions with records` =nEco,
                      `Ecoregions within G buffer` =gEco,
                      `ERS exsitu` = ers)
  # index the selected features
  ids <- ecoSelect[,idColumn][[1]][,1] %in% areasWithOutGBuffer$`ecoID `
  gapEcos <- ecoSelect[ids,]

  # map of results
  terra::plot(gapEcos, main = "Ecoregions outside of the G Buffer areas",
              xlab = "Longitude", ylab = "Latitude")
  # mask the buffers to the sdm
  terra::plot(b1, add = TRUE)
  terra::plot(d1, add = TRUE)

  # export results
  output <- list(
    results = out_df,
    ecoGaps = gapEcos
  )

  # generate dataframe
  return(output)
}

