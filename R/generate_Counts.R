#' @title Generating Species Count Data Frame
#' @name generate_Counts
#' @description This function creates a summary file the summarizes the total number of germplasm and herbarium occurrences
#'
#' @param species The name of a species
#' @param occurrenceData  A data frame object with the species name, geographical coordinates, and type of records (G or H) for a given species
#'
#' @return This function returns a data frame object with the following columns:
#'
#' \tabular{lcc}{
#'  species \tab Species name \cr
#'  totalRecords  \tab Total records available \cr
#'  hasLat  \tab Number of occurrences with latitude \cr
#'  hasLon  \tab Number of occurrences with longitude \cr
#'  totalUseful  \tab Number of occurrences with longitude \cr
#'  totalGRecords  \tab Number of occurrences with longitude \cr
#'  totalGUseful  \tab Number of occurrences with longitude \cr
#'  totalHRecords  \tab Number of occurrences with longitude \cr
#'  totalHUseful  \tab Number of occurrences with longitude \cr
#'  }
#'
#' @examples
#' data("cucurbitaData")
#' ##Obtaining species names from the data
#' speciesList <- unique(cucurbitaData$taxon)
#' countDF <- generate_Counts(speciesList[[1]],cucurbitaData)
#'
#'@references
#'
#' Ramírez-Villegas, J., Khoury, C., Jarvis, A., Debouck, D. G., & Guarino, L. (2010).
#' A Gap Analysis Methodology for Collecting Crop Genepools: A Case Study with Phaseolus Beans.
#' PLOS ONE, 5(10), e13497. Retrieved from https://doi.org/10.1371/journal.pone.0013497
#'
#' Khoury, C. K., Amariles, D., Soto, J. S., Diaz, M. V., Sotelo, S., Sosa, C. C., … Jarvis, A. (2019).
#' Comprehensiveness of conservation of useful wild plants: An operational indicator for biodiversity
#' and sustainable development targets. Ecological Indicators. https://doi.org/10.1016/j.ecolind.2018.11.016
#'
#' @export
#' @importFrom rlang .data
#' @importFrom methods as


generate_Counts <- function(species,occurrenceData){
  taxon <- NULL
  latitude <- NULL
  longitude <- NULL
  hasLat <- NULL
  hasLong <- NULL
  hasLatLong <- NULL
  type <- NULL

  # create an empty dataframe to store counts information
  df <- data.frame(matrix(NA, nrow = 1, ncol = 9))
  colNames <- c("species","totalRecords",	"hasLat", "hasLong","totalUseful", "totalGRecords",
                "totalGUseful","totalHRecords",	"totalHUseful")
  colnames(df) <- colNames

  sppOccAll <- occurrenceData %>%
    dplyr::filter(taxon == species)

      speciesOcc <- occurrenceData %>%
        dplyr::filter(taxon == species)%>%
        dplyr::mutate(hasLat = !is.na(latitude) & latitude != "\\N" & latitude != "" & !is.null(latitude) & latitude != "NULL") %>%
        dplyr::mutate(hasLong = !is.na(longitude) & longitude != "\\N"& longitude != "" & !is.null(longitude)& longitude != "NULL") %>%
        dplyr::mutate(hasLatLong = hasLat & hasLong)
    # group by type and has coordinates
    tbl <- speciesOcc %>%
      dplyr::group_by(type, hasLatLong ) %>%
      dplyr::summarize(total = dplyr::n())
    # assign values to the counts dataframe for the species
    df$species <- as.character(species)
    df$totalRecords <- nrow(speciesOcc)
    df$totalUseful <- sum((subset(tbl, hasLatLong == TRUE))$total)
    df$totalGRecords <- sum((subset(tbl, type == "G"))$total)
    df$totalGUseful <- sum((subset(tbl, type == "G" & hasLatLong == TRUE))$total)
    df$totalHRecords <- sum((subset(tbl, type == "H"))$total)
    df$totalHUseful <- sum((subset(tbl, type == "H" & hasLatLong == TRUE))$total)
    df$hasLat <- sum(speciesOcc$hasLat)
    df$hasLong <- sum(speciesOcc$hasLong)

  # returns the counts dataframe
  return(df)
}
