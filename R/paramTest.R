#'  @title Input Parameter Test
#'
#' @description Preforms a series of test to validation that input occurrence
#'   data and SDM meet require parameters
#'
#' @param Occurrence_data A data frame object with the species name,
#'   geographical coordinates, and type of records (G or H) for a given species
#'
#' @param Raster A raster object representing the species distribution model
#'
#' @return A list with two binary values
#'
#' @examples
#' \dontrun{
#' test <- .paramTest(Occurrence_data = CucurbitaData[CucurbitaData$taxon == "Cucurbita Cordata"],
#'                                  Raster = CucurbitaRasters[[1]])
#' }
#' @author Dan Carver
#'
#' @references
#'
#' @export
#' @keywords internal


paramTest <- function(Occurrence_data, Raster){
  # tests three elements and returns a list of those values
  # these become checks within the function workflow.
  # 1. Are there occurrences
  an <- nrow(Occurrence_data) > 0
  # 2. Are there occurrences with lat long
  oc <- TRUE %in% complete.cases(Occurrence_data[,c("latitude", "longitude")])
  # 3. if no sdm exists
  sd <- class(Raster) == "RasterLayer"
  # 4.no occurrences with an SDM
  noOC <- an == FALSE & sd == TRUE
  # 3. if there are coordinates and sdm exists
  os <- oc == TRUE & sd == TRUE
  return(c(noOC, os))
}
