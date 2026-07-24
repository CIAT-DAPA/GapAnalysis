
#' @title Sampling representativeness score in situ
#' @name SRSin
#' @description The SRSin process calculates the proportion of all occurrences of a taxon falling within
#' the distribution model that also fall within a protected area
#'
#' @param taxon A character object that defines the name of the species as listed in the occurrence dataset
#'
#'
#' @param sdm a terra rast object that
#'
#' @param occurrenceData a data frame of values containing columns for the taxon, latitude, longitude, and type. Coordinates are assumed to be in the WGS84 (EPSG:4326) coordinate reference system.
#'
#' @param protectedAreas A terra rast object the contain spatial location of protected areas.
#'
#' @return A list object containing
#' 1. results : a data frames of values summarizing the results of the function
#' 2. points : a terra vect object showing all the points present within protected areas
#' 3. map : a leaflet object showing the spatial results of the function
#'
#' @examples
#' ##Obtaining occurrences from example
#' data(CucurbitaData)
#' ##Obtaining Raster_list
#' data(CucurbitaRasts)
#' ##Obtaining protected areas raster
#' data(ProtectedAreas)
#'
#' # convert the dataset for function
#' taxon <- "Cucurbita_cordata"
#' sdm <- terra::unwrap(CucurbitaRasts)$cordata
#' occurrenceData <- CucurbitaData
#' protectedAreas <- terra::unwrap(ProtectedAreas)
#' #Running SRSin
#' srs_insitu <- SRSin(taxon = taxon,
#'                     sdm = sdm,
#'                     occurrenceData = occurrenceData,
#'                     protectedAreas = protectedAreas
#'                     )
#'
#' @references
#' Khoury et al. (2019) Ecological Indicators 98:420-429. \doi{10.1016/j.ecolind.2018.11.016}
#' Carver et al. (2021) GapAnalysis: an R package to calculate conservation indicators using spatial information
#' @importFrom terra vect extract
#' @importFrom dplyr filter tibble
#' @importFrom leaflet addTiles addPolygons addLegend addRasterImage addCircleMarkers addControl
#' @export


SRSin <- function(taxon, sdm, occurrenceData, protectedAreas){
  # If an SDM exists:
  #   - keep only occurrence points inside the SDM
  #   - test those for presence in protected areas
  # If no SDM exists:
  #   - use all occurrence points
  #   - test those for presence in protected areas
  # Return the proportion and the visualization of points

  # filter the occurrence data to the species of interest
  d1 <- occurrenceData |>
    dplyr::filter(species == taxon) |>
    terra::vect(geom = c("longitude", "latitude"), crs = "+proj=longlat +datum=WGS84")

  # handle case where there is no SDM model
  if (!inherits(sdm, "SpatRaster")) {
    p1 <- d1
    p1$inSDM <- NA
  } else {
    # extract values from the sdm
    d1$inSDM <- terra::extract(sdm, d1, ID = FALSE)

    # points in sdm
    p1 <- d1[d1$inSDM == 1, ]
  }

  # extract vals from protected layers
  if (nrow(p1) > 0) {
    p1$inPro <- terra::extract(protectedAreas, p1, ID = FALSE)

    # points in protected areas
    protectedPoints <- p1[p1$inPro == 1, ]
  } else {
    p1$inPro <- numeric(0)
    protectedPoints <- p1
  }

  # srsin
  if (nrow(p1) > 0) {
    srsin <- nrow(protectedPoints) / nrow(p1) * 100
  } else {
    srsin <- 0
  }

  # dataframe for export
  out_df <- dplyr::tibble(
    Taxon = taxon,
    "Total Observations" = nrow(d1),
    "Total records in SDM" = if (inherits(sdm, "SpatRaster")) nrow(p1) else NA_integer_,
    "Records in Protected areas" = nrow(protectedPoints),
    "SRS insitu" = srsin
  )

  # quick map
  if (nrow(p1) > 0) {
    p1$color <- ifelse(is.na(p1$inPro), "#444444", "#746fae")
  }

  map_title <- if (inherits(sdm, "SpatRaster")) {
    "<h3 style='text-align:center; background-color:rgba(255,255,255,0.7); padding:2px;'>Points within SDM inside of protected areas</h3>"
  } else {
    "<h3 style='text-align:center; background-color:rgba(255,255,255,0.7); padding:2px;'>Occurrence points inside of protected areas (no SDM available)</h3>"
  }

  map <- leaflet::leaflet() |>
    leaflet::addTiles()

  if (inherits(sdm, "SpatRaster")) {
    map <- map |>
      leaflet::addRasterImage(
        x = sdm,
        colors = "#47ae24"
      )
  }

  if (nrow(p1) > 0) {
    map <- map |>
      leaflet::addCircleMarkers(
        data = p1,
        color = ~color,
        radius = 1,
        opacity = 1
      )
  }

  map <- map |>
    leaflet::addLegend(
      position = "topright",
      title = "SRS in situ",
      colors = if (inherits(sdm, "SpatRaster")) c("#47ae24", "#746fae", "#444444") else c("#746fae", "#444444"),
      labels = if (inherits(sdm, "SpatRaster")) {
        c("Distribution", "Protected Occurrences", "Non Protected Occurrences")
      } else {
        c("Protected Occurrences", "Non Protected Occurrences")
      },
      opacity = 1
    ) |>
    leaflet::addControl(html = map_title, position = "bottomleft")

  # define output
  output <- list(
    results = out_df,
    points = p1,
    map = map
  )
  return(output)
}
