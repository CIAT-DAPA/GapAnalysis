
#'
#' @title Ecological representativeness score in situ
#' @name ERSin
#' @description The ERSin process provides an ecological measurement of the proportion of a species range
#'  that can be considered to be conserved in protected areas. The ERSin calculates the proportion of ecoregions
#'  encompassed within the range of the taxon located inside protected areas to the ecoregions encompassed
#'  within the total area of the distribution model, considering comprehensive conservation to have been accomplished
#'  only when every ecoregion potentially inhabited by a species is included within the distribution of the species
#'  located within a protected area.
#' @param taxon
#'
#' @param sdm
#' @param occurrence_Data
#' @param protectedAreas
#' @param ecoregions
#' @param idColumn
#'
#' @return
#'
#' @examples
#'
#'
#' @references
#' Khoury et al. (2019) Ecological Indicators 98:420-429. doi: 10.1016/j.ecolind.2018.11.016
#' Carver et al. (2021) GapAnalysis: an R package to calculate conservation indicators using spatial information
ERSin <- function(taxon, sdm, occurrence_Data, protectedAreas, ecoregions, idColumn) {
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
  eco$totEco <- terra::zonal(x = sdm , z = eco, fun = "sum",na.rm=TRUE) |> pull()
  selectedEcos <- eco[eco$totEco > 0 , ]
  nEcoModel <- nrow(selectedEcos)
  # Get ecoregions in pro areas
  eco$totPro <- terra::zonal(x = sdm , z = eco, fun = "sum",na.rm=TRUE) |> pull()
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
  map <- leaflet() |>
    addTiles() |>
    addPolygons(data = selectedEcos,
                color = "#444444",
                weight = 1,
                opacity = 1.0,
                popup = ~ECO_NAME,
                fillOpacity = 0.5,
                fillColor = "#44444420")|>
    addPolygons(data = missingEcos,
                color = "#444444",
                weight = 1,
                opacity = 1.0,
                popup = ~ECO_NAME,
                fillOpacity = 0.5,
                fillColor = "#f0a01f")|>
    addRasterImage(
      x = sdm,
      colors = "#47ae24"
    ) |>
    addRasterImage(
      x = proMask,
      colors = "#746fae"
    )|>
    addLegend(
      position = "topright",
      title = "ERS in situ",
      colors = c("#47ae24","#746fae", "#f0a01f", "#44444440"),
      labels = c("Distribution","Protected Areas", "Eco gaps", "All Ecos"),
      opacity = 1
    )|>
    addControl(html = map_title, position = "bottomleft")





  # output
  output = list(
    results = df,
    missingEcos = missingEcos,
    map = map
  )
  return(output)
}
