#' @title Generating counts dataframe for taxa
#' @name OccurrenceCounts
#' @description This function creates a summary file counting
#'  the total number of G and H occurrences, including those with coordinates
#'
#' @param species A vector of characters with the species name.
#' @param Occurrence_data A data frame object with the species name, geographical coordinates,
#'  and type of records (G or H) for a given species
#' @return This function returns a data frame object with the following columns:
#'
#' \tabular{lcc}{
#'  species \tab Species name \cr
#'  totalRecords  \tab Total number of records  \cr
#'  hasLat  \tab Number of occurrences with latitude \cr
#'  hasLon  \tab Number of occurrences with longitude \cr
#'  totalUseful  \tab Number of occurrences with coordinates \cr
#'  totalGRecords  \tab Number of G occurrences \cr
#'  totalGUseful  \tab Number of G occurrences with coordinates \cr
#'  totalHRecords  \tab Number of H occurrences \cr
#'  totalHUseful  \tab Number of H occurrences with coordinates \cr
#'  }
#'
#' @examples
#' data(CucurbitaData)
#' ##Obtaining species names from the data
#' Cucurbita_splist <- unique(CucurbitaData$species)
#' sp_counts <- OccurrenceCounts(Cucurbita_splist[[1]],CucurbitaData)
#'
#'@references
#'
#' Ramirez-Villegas et al. (2010) PLOS ONE, 5(10), e13497. doi: 10.1371/journal.pone.0013497
#' Khoury et al. 2019) Ecological Indicators 98: 420-429. doi: 10.1016/j.ecolind.2018.11.016
#'
#' @export
#' @keywords internal


OccurrenceCounts <- function(species,Occurrence_data){
  taxon <- NULL
  latitude <- NULL
  longitude <- NULL
  hasLat <- NULL
  hasLong <- NULL
  hasLatLong <- NULL
  type <- NULL

  #Checking Occurrence_data format
  par_names <- c("species","latitude","longitude","type")

  if(isFALSE(identical(names(Occurrence_data),par_names))){
    stop("Please format the column names in your dataframe as species, latitude, longitude, type")
  }

  # create an empty dataframe to store counts information
  df <- data.frame(matrix(NA, nrow = 1, ncol = 9))
  colNames <- c("species","totalRecords",	"hasLat", "hasLong","totalCoords", "totalGRecords",
                "totalGCoords","totalHRecords",	"totalHCoords")
  colnames(df) <- colNames

  speciesOcc <- Occurrence_data[which(Occurrence_data$species==species),]
  speciesOcc$hasLat <- !is.na(speciesOcc$latitude) &
  speciesOcc$latitude != "\\N" & speciesOcc$latitude != "" &
    !is.null(speciesOcc$latitude) & speciesOcc$latitude != "NULL"

  speciesOcc$hasLong <- !is.na(speciesOcc$longitude) &
  speciesOcc$longitude != "\\N" & speciesOcc$longitude != "" &
    !is.null(speciesOcc$longitude) & speciesOcc$longitude != "NULL"

  speciesOcc$hasLatLong <- speciesOcc$hasLat==speciesOcc$hasLong

  # group by type and has coordinates
  tbl <- stats::aggregate(speciesOcc,list(type   = speciesOcc$type,hasLatLong =speciesOcc$hasLatLong), length)
  tbl <- tbl[,c("type","hasLatLong","species")]; colnames(tbl)[3] <- "total"

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
