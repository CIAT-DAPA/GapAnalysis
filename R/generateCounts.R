#' @title Generate initial counts of the occurrence data
#' @name generateCounts
#' @description
#' Performs data cleaning to generate a summary of all input occurrence data. These values area
#' used in the SRSex function.
#'
#' @param taxon A character object that defines the name of the species as listed in the occurrence dataset
#' @param occurrence_Data a data frame of values containing columns for the taxon, latitude, longitude, and type
#'
#' @return countsData : a data frames of values summarizing the results of the function
#'
#' @examples
#' ##Obtaining occurrences from example
#' data(CucurbitaData)
#'
#' # convert the dataset for function
#' taxon <- "Cucurbita_cordata"
#' occurrenceData <- CucurbitaData
#'
#' #Running generateCounts
#' counts <- generateCounts(taxon = taxon,
#'                     occurrence_Data = occurrenceData
#'                     )
#'
#' @references
#' Khoury et al. (2019) Ecological Indicators 98:420-429. doi: 10.1016/j.ecolind.2018.11.016
#' Carver et al. (2021) GapAnalysis: an R package to calculate conservation indicators using spatial information
#' @importFrom dplyr filter select mutate group_by summarize n
#' @export

generateCounts <- function(taxon, occurrence_Data){

  # define presence of usable lat long values
  dataThin <- occurrence_Data |>
    dplyr::filter(species == taxon) |>
    dplyr::select(c("species", "latitude", "longitude", "type")) |>
    dplyr::mutate(hasLat = !is.na(latitude) & latitude != "\\N" & latitude != "" & !is.null(latitude) & latitude != "NULL") |>
    dplyr::mutate(hasLong = !is.na(longitude) & longitude != "\\N"& longitude != "" & !is.null(longitude)& longitude != "NULL") |>
    dplyr::mutate(hasLatLong = hasLat & hasLong)

  # set column names for counts df
  colNames <- c("species","totalRecords",	"hasLat", "hasLong","totalUseful", 	"totalGRecords",
                "totalGUseful","totalHRecords",	"totalHUseful")
  # summarize data
  tbl <- dataThin |>
    dplyr::group_by(type, hasLatLong )|>
    dplyr::summarize(total = dplyr::n())

  # generate counts df
  countsData <- data.frame(matrix(NA, nrow = 1, ncol = 9))
  colnames(countsData) <- colNames
  #assign values to counts df
  countsData$species <- taxon
  countsData$totalRecords <- nrow(dataThin)
  countsData$totalUseful <- sum((subset(tbl, hasLatLong == TRUE))$total)
  countsData$totalGRecords <- sum((subset(tbl, type == "G"))$total)
  countsData$totalGUseful <- sum((subset(tbl, type == "G" & hasLatLong == TRUE))$total)
  countsData$totalHRecords <- sum((subset(tbl, type == "H"))$total)
  countsData$totalHUseful <- sum((subset(tbl, type == "H" & hasLatLong == TRUE))$total)
  countsData$hasLat <- sum(dataThin$hasLat)
  countsData$hasLong <- sum(dataThin$hasLong)
  return(countsData)
}
