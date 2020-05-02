#' @title In situ gap analysis calculation (In situ conservation)
#' @name insitu_GapAnalysis
#' @description This function allows calculate all the three In situ gap analysis scores in one unique function returning a final conservation score summary table
#'
#' @param species_list An species list to calculate metrics.
#' @param occurrenceData A data frame object with the species name, geographical coordinates, and type of records (G or H) for a given species
#' @param raster_list A list representing the species distribution models for the species list provided loaded in raster format. This list must match the same order of the species list.
#' @param proArea A raster file representing protected areas information. If proArea=NULL the funtion will use a protected area raster file
#'  provided for your use after run preparing_Datasets()
#' @param ecoReg A shapefile representing ecoregions information with a field ECO_NUM representing ecoregions Ids. If ecoReg=NULL the funtion will use a shapefile
#'  provided for your use after run preparing_Datasets()
#'
#' @return This function returns a data frame with the follow information summarizing the ex-situ gap analysis scores:
#'
#' \tabular{lcc}{
#' species \tab Species name \cr
#' SRSex \tab Ex situ sample representativeness score \cr
#' GRSex \tab Ex situ germplasm representativeness score \cr
#' ERSex \tab Ex situ environmental representativeness score \cr
#' FCSex \tab Ex situ final conservation score \cr
#' }
#'
#' @examples
#' ##Obtaining occurrences from example
#' data("cucurbitaData")
#' ##Obtaining species names from the data
#' speciesList <- unique(cucurbitaData$taxon)
#' ##Obtaining raster_list
#' data("sdm_rasters")
#' ##Obtaining protected areas raster
#' data("protectedArea")
#' ##Obtaining ecoregions shapefile
#' data("ecoregions")
#'
#' #Running all three In situ gap analysis steps using insituGapAnalysis function
#' insituGapMetrics <- insitu_GapAnalysis(species_list=speciesList,
#'                                        occurrenceData=cucurbitaData,
#'                                        raster_list=sdm_rasters,
#'                                        proArea=protectedArea,
#'                                        ecoReg=ecoregions)
#'
#' @references
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

insitu_GapAnalysis <- function(species_list, occurrenceData, raster_list,proArea,ecoReg){
  # call SRSin
  srsIn <- gapAnalysisR::srs_Insitu(species_list = species_list,
    occurrenceData = occurrenceData,
     raster_list = raster_list,
    proArea=proArea)
  # call GRSin
  grsIn <- gapAnalysisR::grs_Insitu(species_list = species_list,
    occurrenceData = occurrenceData,
    raster_list = raster_list,
    proArea=proArea)
  # call ERSin
  ersIn <- gapAnalysisR::ers_Insitu(species_list = species_list,
    occurrenceData =occurrenceData,
    raster_list = raster_list,
    proArea=proArea,
    ecoReg=ecoReg)

  # call FCSex
  fcsIn <- gapAnalysisR::fcs_Insitu(srsIn,grsIn,ersIn)

  # return dataframe from FCSex
  return(fcsIn)
}
