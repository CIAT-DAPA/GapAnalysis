#' @title IUCN conservations status using  Area of occupancy (AOO)
#'  and extent of occurrence (EOO) (IUCN Redlist)
#' @name eooAoo
#' @author Dan Carver
#' @description This function calculates the species conservation status
#'  according IUCN parameters using Area of occupancy (AOO) Area
#'  and extent of occurrence (EOO)
#'
#' @param species_list An species list to calculate metrics.
#' @param occurrenceData A data frame object with the species name,
#'  geographical coordinates, and type of records (G or H) for a given species
#'
#' @return This function returns a data frame with the following columns:
#'
#' \tabular{lcc}{
#'  species \tab Species name \cr
#'  EOO Status \tab IUCN conservation status (EOO)  \cr
#'  AOO Status \tab IUCN conservation status (AOO) \cr
#' }
#' @references
#'  Bland, Lucie & Keith, David & Miller, Rebecca & Murray,
#' Nicholas & Rodríguez, Jon. (2017). Guidelines for the application of
#' IUCN Red List of Ecosystems Categories and Criteria,
#' version 1.1. 10.2305/IUCN.CH.2016.RLE.3.en.
#'
#'  Lee, C. K. F., Keith, D. A., Nicholson, E. and Murray, N. J. 2019.
#' Redlistr: tools for the IUCN Red Lists of ecosystems and threatened species in R.
#'  – Ecography 42: 1050–1055 (ver. 0).
#' @examples
#' ##Obtaining occurrences from example
#' data(CucurbitaData)
#' ##Obtaining species names from the data
#' speciesList <- unique(CucurbitaData$taxon)
#' ## Obtaining AOO and EOO ##
#' eooAoo <- GapAnalysis::eooAoo(species_list = speciesList,
#'                                occurrenceData = CucurbitaData)
#'
#' @export
#' @importFrom magrittr %>%
#' @importFrom raster crs
#' @importFrom dplyr filter
#' @importFrom redlistr makeEOO getAreaEOO makeAOOGrid gridUncertainty
#' @import sp



eooAoo <- function(species_list, occurrenceData){
  df <- NULL
  taxon <- NULL
  df <- data.frame(matrix(ncol=3, nrow = length(species_list)))
  colnames(df) <- c("species", "EOO Status", "AOO Status")
  # loop over species list
  for(i in seq_len(length(species_list))){

    wgs84 <- raster::crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
    worldEqualArea <- raster::crs("+proj=cea +lon_0=0 +lat_ts=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs ")

    #filter out species occurrence data with coordinates
    ocd <-occurrenceData %>%
      dplyr::filter(taxon == species_list[i])
    ocd <- ocd[stats::complete.cases(ocd),]
    coords <- ocd[,c("longitude","latitude")]
    cleanPoints2 <- sp::SpatialPoints(coords = coords, proj4string = wgs84)
    spAllPro <- sp::spTransform(cleanPoints2, worldEqualArea)
    EOO.polygon <- redlistr::makeEOO(spAllPro)
    # then calcualte the area of the bounding box
    EOO.area <- redlistr::getAreaEOO(EOO.polygon)
    #determine status based on area
    if (EOO.area >= 45000) {blo <- "Least Concern (LC)"}
    if (EOO.area < 45000) {blo <- "Possible Near Threatened (NT)"}
    if (EOO.area < 20000) {blo <- "Vulnerable (VU)"} # 20000
    if (EOO.area < 5000) {blo <- "Endangered (EN)"} # 5000
    if (EOO.area < 100) {blo <- "Critically Endangered (CR)"} # 100
    if (EOO.area == "NA") {blo <- "Critically Endangered (CR)"}

    #EOO.area
    # this value is then use in the develop of other criteria in the sebcriterion B1

    ### Subcriterion B2 (calculating AOO)
    # create a 10 x 10 grid of to overlay on distribution.

    AOO.grid <- redlistr::makeAOOGrid(spAllPro, grid.size = 4000,
                                      min.percent.rule = FALSE)
    #plot(AOO.grid)
    n.AOO <- length(AOO.grid)
    AOOarea <- n.AOO* 4
    if (AOOarea >= 4500) {AOO_cat <- "Least Concern (LC)"} # <
    if (AOOarea < 4500) {AOO_cat <- "Possible Near Threatened (NT)"}
    if (AOOarea < 2000) {AOO_cat <- "Vulnerable (VU)"} # < 2000
    if (AOOarea < 500) {AOO_cat <- "Endangered (EN)"}# < 500
    if (AOOarea < 10) {AOO_cat  <- "Critically Endangered (CR)"}# < 10
    if (AOOarea == "NA") {AOO_cat <- "Critically Endangered (CR)"}


    #n.AOO
    # so the length is just the number of grid cells that overlay this environment
    # because the position of the grid cells can potential change the number of cells
    # a randomized process is used to determine a minimun number of grids.

    gU.results <- redlistr::gridUncertainty(spAllPro, 4000,
                                            n.AOO.improvement = 1,
                                            min.percent.rule = FALSE)

    df$species[i] <- as.character(species_list[i])
    df$`EOO Status`[i] <- blo
    df$`AOO Status`[i] <- AOO_cat
  }
  return(df)
}
