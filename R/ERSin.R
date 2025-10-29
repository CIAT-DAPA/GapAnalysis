#' @title Ecological representativeness score in situ
#' @name ERSin
#' @description The ERSin process provides an ecological measurement of the proportion of a species range
#'  that can be considered to be conserved in protected areas. The ERSin calculates the proportion of ecoregions
#'  encompassed within the range of the taxon located inside protected areas to the ecoregions encompassed
#'  within the total area of the distribution model, considering comprehensive conservation to have been accomplished
#'  only when every ecoregion potentially inhabited by a species is included within the distribution of the species
#'  located within a protected area.
#'
#' @param taxon A character object that defines the name of the species as listed in the occurrence dataset
#' @param sdm a terra rast object that represented the expected distribution of the species
#' @param occurrenceData a data frame of values containing columns for the taxon, latitude, longitude, and type
#' @param protectedAreas A terra rast object the contian spatial location of protected areas.
#' @param ecoregions A terra vect object the contains spatial information on all ecoregions of interests
#' @param idColumn A character vector that notes what column within the ecoregions object should be used as a unique ID
#'
#'#' @return A list object containing
#' 1. results : a data frames of values summarizing the results of the function
#' 2. missingEcos : a terra vect object showing all the ecoregions within the distribution with no protected areas present
#' 3. map : a leaflet object showing the spatial results of the function
#'
#'
#' @examples
#' ##Obtaining occurrences from example
#' data(CucurbitaData)
#' ##Obtaining Raster_list
#' data(CucurbitaRasts)
#' ##Obtaining protected areas raster
#' data(ProtectedAreas)
#' ## ecoregion features
#' data(ecoregions)
#'
#' # convert the dataset for function
#' taxon <- "Cucurbita_cordata"
#' sdm <- terra::unwrap(CucurbitaRasts)$cordata
#' protectedAreas <- terra::unwrap(ProtectedAreas)
#' ecoregions <- terra::vect(ecoregions)
#'
#' #Running ERSin
#' ers_insitu <- ERSin(taxon = taxon,
#'                     sdm = sdm,
#'                     occurrenceData = CucurbitaData,
#'                     protectedAreas = protectedAreas,
#'                     ecoregions = ecoregions,
#'                     idColumn = "ECO_NAME"
#'                     )
#'
#'
#' @references
#' Khoury et al. (2019) Ecological Indicators 98:420-429. doi: 10.1016/j.ecolind.2018.11.016
#' Carver et al. (2021) GapAnalysis: an R package to calculate conservation indicators using spatial information
#' @importFrom dplyr tibble pull
#' @importFrom terra crop aggregate zonal
#' @importFrom leaflet addTiles addPolygons addLegend addRasterImage addCircleMarkers
#' @importFrom magrittr %>%
#' @export

ERSin <- function(taxon, sdm, occurrenceData, protectedAreas, ecoregions, idColumn) {
  # crop protected areas to sdm
  pro <- terra::crop(protectedAreas, sdm)
  # mask to model
  proMask <- pro * sdm
  # set id column for easier indexing
  ecoregions$id_column <- as.data.frame(ecoregions)[,idColumn]
  # aggregates spatial features
  ecoregions <- terra::aggregate(x = ecoregions, by = "id_column")

  # crop ecos to sdm
  eco <- terra::crop(ecoregions, sdm)

  # Get ecoregions in sdm
  eco$totEco <- terra::zonal(x = sdm , z = eco, fun = "sum",na.rm=TRUE) |> dplyr::pull()
  selectedEcos <- eco[eco$totEco > 0 , ]
  nEcoModel <- nrow(selectedEcos)
  # Get ecoregions in pro areas
  eco$totPro <- terra::zonal(x = proMask , z = eco, fun = "sum",na.rm=TRUE) |> dplyr::pull()
  protectedEcos <- eco[eco$totPro > 0 , ]
  nProModel <- nrow(protectedEcos)
  # get missing ecos
  missingEcos <- selectedEcos[!selectedEcos$id_column %in% protectedEcos$id_column, ]


  # calculate the ers
  if(nProModel == 0){
    ers <- 0
  }else{
    ers <- (nProModel/nEcoModel)*100
  }
  #results
  df <- dplyr::tibble(Taxon = taxon,
                   "Ecoregions within model" = nEcoModel,
                   "Ecoregions with protected areas" = nProModel,
                   "ERS insitu" = ers)
  # generate the base map
  map_title <- "<h3 style='text-align:center; background-color:rgba(255,255,255,0.7); padding:2px;'>Ecoregions within the SDM without Protected Area</h3>"
  map <- leaflet::leaflet() |>
    leaflet::addTiles() |>
    leaflet::addPolygons(data = selectedEcos,
                color = "#444444",
                weight = 1,
                opacity = 1.0,
                popup = ~ECO_NAME,
                fillOpacity = 0.5,
                fillColor = "#44444420")|>
    leaflet::addPolygons(data = missingEcos,
                color = "#444444",
                weight = 1,
                opacity = 1.0,
                popup = ~ECO_NAME,
                fillOpacity = 0.5,
                fillColor = "#f0a01f")|>
    leaflet::addRasterImage(
      x = sdm,
      colors = "#47ae24"
    ) |>
    leaflet::addRasterImage(
      x = proMask,
      colors = "#746fae"
    )|>
    leaflet::addLegend(
      position = "topright",
      title = "ERS in situ",
      colors = c("#47ae24","#746fae", "#f0a01f", "#44444440"),
      labels = c("Distribution","Protected Areas", "Eco gaps", "All Ecos"),
      opacity = 1
    )|>
    leaflet::addControl(html = map_title, position = "bottomleft")





  # output
  output = list(
    results = df,
    missingEcos = missingEcos,
    map = map
  )
  return(output)
}
