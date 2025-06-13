#' @title Download datasets from Dataverse
#' @name GetDataSets
#' @description
#' A short description...
#'
#' @param
#'
#' @return
#'
#' @examples
#'
#'
#' @references
#' Khoury et al. (2019) Ecological Indicators 98:420-429. doi: 10.1016/j.ecolind.2018.11.016
#' Carver et al. (2021) GapAnalysis: an R package to calculate conservation indicators using spatial information
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
    dplyr::summarize(total = n())

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
