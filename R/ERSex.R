#' @title Download datasets from Dataverse
#' @name ERSex
#' @description The ERSex process provides an ecological measurement of the proportion of a species
#'  range that can be considered to be conserved in ex situ repositories. The ERSex calculates the
#'  proportion of terrestrial ecoregions (The Nature Conservancy Geospatial Conservation Atlas 2019)
#'  represented within the G buffered areas out of the total number of ecoregions occupied by the distribution model.
#'
#'
#' @param taxon A character object that defines the name of the species as listed in the occurrence dataset
#' @param sdm a terra rast object that represented the expected distribution of the species
#' @param occurrenceData a data frame of values containing columns for the taxon, latitude, longitude, and type
#' @param gBuffer A terra vect which encompases a specific buffer distance around all G points
#' @param ecoregions A terra vect object the contains spatial information on all ecoregions of interests
#' @param idColumn A character vector that notes what column within the ecoregions object should be used as a unique ID
#'
#'
#'
#' @return A list object containing
#' 1. results : a data frames of values summarizing the results of the function
#' 2. ecogaps : a terra vect object showing the ecoregions with no area within the g buffer objects
#' 3. map : a leaflet object showing the spatial results of the function
#'
#'
#' @examples
#' ##Obtaining occurrences from example
#' data(CucurbitaData)
#' ##Obtaining Raster_list
#' data(CucurbitaRasts)
#' ## ecoregion features
#' data(ecoregions)
#'
#' # convert the dataset for function
#' taxon <- "Cucurbita_cordata"
#' sdm <- terra::unwrap(CucurbitaRasts)$cordata
#' ecoregions <- terra::vect(ecoregions)
#' #Running generateGBuffers
#' gBuffer <- generateGBuffers(taxon = taxon,
#'                     occurrenceData = CucurbitaData,
#'                     bufferDistM = 50000
#'                     )
#' #Running ERSex
#' ers_exsitu <- ERSex(taxon = taxon,
#'                     sdm = sdm,
#'                     occurrenceData = CucurbitaData,
#'                    gBuffer = gBuffer,
#'                    ecoregions = ecoregions,
#'                    idColumn = "ECO_NAME"
#'                    )
#'
#'
#' @references
#' Khoury et al. (2019) Ecological Indicators 98:420-429. doi: 10.1016/j.ecolind.2018.11.016
#' Carver et al. (2021) GapAnalysis: an R package to calculate conservation indicators using spatial information
#' @importFrom dplyr filter mutate tibble summarise case_when
#' @importFrom terra vect crop aggregate zonal rasterize as.data.frame
#' @importFrom leaflet addTiles addPolygons addLegend addRasterImage addCircleMarkers
#' @export

ERSex <- function(taxon, sdm, occurrenceData, gBuffer, ecoregions, idColumn) {
  # filter the occurrence data to the species of interest
  d1 <- occurrenceData |>
    dplyr::filter(occurrenceData$species == taxon) |>
    terra::vect(
      geom = c("longitude", "latitude"),
      crs = "+proj=longlat +datum=WGS84"
    )
  # add color
  d1$color <- ifelse(d1$type == "H", yes = "#1184d4", no = "#6300f0")
  # set id column for easier indexing
  ecoregions$id_column <- as.data.frame(ecoregions)[[idColumn]]
  # aggregates spatial features
  ecoregions <- terra::aggregate(x = ecoregions, by = "id_column")

  # determine the eco regions present in the
  ## crop ecos
  ecoregions <- terra::crop(ecoregions, sdm)
  ecoregions$sdmSum <- terra::zonal(
    x = sdm,
    z = ecoregions,
    fun = "sum",
    na.rm = TRUE
  )
  # subset ecoregions to feautres with greated then 0
  ecoSelect <- ecoregions[ecoregions$sdmSum > 0, ]
  # # index with selection
  # ## conver to table for easier indexing
  eco2 <- terra::as.data.frame(ecoSelect) |>
    dplyr::select(ecoID = id_column, count = sdmSum)

  # condition for no G points
  if (is.character(gBuffer$data)) {
    ers <- 0
    gEco <- NA
    gEcoCounts <- 0
    totalEcosCount <- nrow(ecoSelect)
    missingEcos <- eco2$ecoID
  } else {
    # rasterize the buffer object
    b1 <- terra::rasterize(x = gBuffer$data, y = sdm) |> terra::mask(sdm)
    # determine ecoregions in ga50 area
    eco2$bufferEcos <- terra::zonal(
      x = b1,
      z = ecoSelect,
      fun = "sum",
      na.rm = TRUE
    ) |>
      unlist()
    # group by data to get single value per ecoregion
    ecoGrouped <- eco2 |>
      dplyr::mutate(
        bufferEcos = dplyr::case_when(
          is.na(bufferEcos) ~ 0,
          is.nan(bufferEcos) ~ 0,
          TRUE ~ bufferEcos
        )
      ) |>
      dplyr::group_by(ecoID) |>
      dplyr::summarise(
        inDistribution = sum(count, na.rm = TRUE),
        inGBuffer = sum(bufferEcos, na.rm = TRUE)
      )
    # total eco
    totalEcosCount <- nrow(ecoGrouped)
    # ecoregions with coverage
    gEcoIds <- ecoGrouped[ecoGrouped$inGBuffer > 0, "ecoID"] |> pull()
    gEcoCounts <- length(gEcoIds)
    # select map elements
    missingEcos <- ecoSelect[!ecoSelect$id_column %in% gEcoIds, ]
    # ERs calculation
    ers <- min(c(100, (gEcoCounts / totalEcosCount) * 100))
  }

  # generate filter
  out_df = dplyr::tibble(
    Taxon = taxon,
    `Ecoregions with records` = totalEcosCount,
    `Ecoregions within G buffer` = gEcoCounts,
    `ERS exsitu` = ers
  )

  # leaflet map of results
  map_title <- "<h3 style='text-align:center; background-color:rgba(255,255,255,0.7); padding:2px;'>Ecoregions outside of the G Buffer areas</h3>"
  # base map element
  map <- leaflet() |>
    leaflet::addTiles() |>
    leaflet::addPolygons(
      data = ecoSelect,
      color = "#444444",
      weight = 1,
      opacity = 1.0,
      fillOpacity = 0.1,
      popup = ~ECO_NAME,
      fillColor = NA
    ) |>
    leaflet::addLegend(
      position = "topright",
      title = "ERS ex situ",
      colors = c("#47ae24", "#746fae", "#f0a01f", "#44444440"),
      labels = c("Distribution", "G buffer", "Eco gaps", "All Ecos"),
      opacity = 1
    ) |>
    leaflet::addControl(html = map_title, position = "bottomleft")

  if (ers > 0) {
    # add additional map elements
    map <- map |>
      leaflet::addPolygons(
        data = missingEcos,
        color = "#444444",
        weight = 1,
        opacity = 1.0,
        popup = ~ECO_NAME,
        fillOpacity = 0.5,
        fillColor = "#f0a01f"
      ) |>
      leaflet::addRasterImage(
        x = sdm,
        colors = "#47ae24"
      ) |>
      leaflet::addRasterImage(
        x = b1,
        colors = "#746fae"
      ) |>
      leaflet::addCircleMarkers(
        data = d1,
        color = ~color,
        radius = 2,
        opacity = 1
      )
  } else {
    map <- map |>
      leaflet::addCircleMarkers(
        data = d1,
        color = ~color,
        radius = 2,
        opacity = 1
      )
  }

  # export results
  output <- list(
    results = out_df,
    ecoGaps = missingEcos,
    map = map
  )

  # generate dataframe
  return(output)
}
