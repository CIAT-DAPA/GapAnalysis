#' @title Generating Species Count Data Frame
#' @name OccurrenceCounts
#' @description This function creates a summary file the summarizes
#'  the total number of germplasm and herbarium occurrences
#'
#' @param species The name of a species
#' @param Occurrence_data  A data frame object with the species name, g
#'  geographical coordinates, and type of records (G or H) for a given species
#'
#' @return This function returns a data frame object with the following columns:
#'
#' \tabular{lcc}{
#'  species \tab Species name \cr
#'  totalRecords  \tab Total records available \cr
#'  hasLat  \tab Number of occurrences with latitude \cr
#'  hasLon  \tab Number of occurrences with longitude \cr
#'  totalUseful  \tab Number of occurrences with latitude and longitude \cr
#'  totalGRecords  \tab Number of G occurrences \cr
#'  totalGUseful  \tab Number of occurrences with latitude and longitude \cr
#'  totalHRecords  \tab Number of H occurrences \cr
#'  totalHUseful  \tab Number of occurrences with latitude and longitude \cr
#'  }
#'
#' @examples
#' data(CucurbitaData)
#' ##Obtaining species names from the data
#' Cucurbita_splist <- unique(CucurbitaData$taxon)
#' countDF <- OccurrenceCounts(Cucurbita_splist[[1]], CucurbitaData)
#'
#'@references
#'
#' Ramirez-Villegas, J., Khoury, C., Jarvis, A., Debouck, D. G., & Guarino, L. (2010).
#' A Gap Analysis Methodology for Collecting Crop Genepools: A Case Study with Phaseolus Beans.
#' PLOS ONE, 5(10), e13497. Retrieved from https://doi.org/10.1371/journal.pone.0013497
#'
#' Khoury, C. K., Amariles, D., Soto, J. S., Diaz, M. V., Sotelo, S., Sosa, C. C., … Jarvis, A. (2019).
#' Comprehensiveness of conservation of useful wild plants: An operational indicator for biodiversity
#' and sustainable development targets. Ecological Indicators. https://doi.org/10.1016/j.ecolind.2018.11.016
#'
#' @export


OccurrenceCounts <- function(species,Occurrence_data){
  taxon <- NULL
  latitude <- NULL
  longitude <- NULL
  hasLat <- NULL
  hasLong <- NULL
  hasLatLong <- NULL
  type <- NULL

  #Checking Occurrence_data format
  par_names <- c("taxon","latitude","longitude","type")

  if(identical(names(Occurrence_data),par_names)==FALSE){
    stop("Please format the column names in your dataframe as taxon,latitude,longitude,type")
  }

  # create an empty dataframe to store counts information
  df <- data.frame(matrix(NA, nrow = 1, ncol = 9))
  colNames <- c("species","totalRecords",	"hasLat", "hasLong","totalCoords", "totalGRecords",
                "totalGCoords","totalHRecords",	"totalHCoords")
  colnames(df) <- colNames

  speciesOcc <- Occurrence_data[which(Occurrence_data$taxon==species),]
  speciesOcc$hasLat <- !is.na(speciesOcc$latitude) &
  speciesOcc$latitude != "\\N" & speciesOcc$latitude != "" &
    !is.null(speciesOcc$latitude) & speciesOcc$latitude != "NULL"

  speciesOcc$hasLong <- !is.na(speciesOcc$longitude) &
  speciesOcc$longitude != "\\N" & speciesOcc$longitude != "" &
    !is.null(speciesOcc$longitude) & speciesOcc$longitude != "NULL"

  speciesOcc$hasLatLong <- speciesOcc$hasLat==speciesOcc$hasLong

  # group by type and has coordinates
  tbl <- stats::aggregate(speciesOcc,list(type   = speciesOcc$type,hasLatLong =speciesOcc$hasLatLong), length)
  tbl <- tbl[,c("type","hasLatLong","taxon")]; colnames(tbl)[3] <- "total"

   # assign values to the counts dataframe for the species
  df$species <- as.character(species)
  df$totalRecords <- nrow(speciesOcc)
  df$totalCoords <- sum((subset(tbl, hasLatLong == TRUE))$total)
  df$totalGRecords <- sum((subset(tbl, type == "G"))$total)
  df$totalGCoords <- sum((subset(tbl, type == "G" & hasLatLong == TRUE))$total)
  df$totalHRecords <- sum((subset(tbl, type == "H"))$total)
  df$totalHCoords <- sum((subset(tbl, type == "H" & hasLatLong == TRUE))$total)
  df$hasLat <- sum(speciesOcc$hasLat)
  df$hasLong <- sum(speciesOcc$hasLong)

  # returns the counts dataframe
  return(df)
}
