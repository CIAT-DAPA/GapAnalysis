#' @title In-situ gap analysis calculation (In-situ conservation)
#' @name InsituCompile
#' @description This function allows calculating all the three In-situ gap analysis scores in one unique function returning a final conservation score summary table
#'
#' @param species_list An species list to calculate metrics.
#' @param occurrenceData A data frame object with the species name, geographical coordinates, and type of records (G or H) for a given species
#' @param raster_list A list representing the species distribution models for the species list provided loaded in raster format. This list must match the same order of the species list.
#' @param proArea A raster file representing protected areas information. If proArea=NULL the function will use a protected area raster file
#'  provided for your use after run GetDatasets()
#' @param ecoReg A shapefile representing ecoregions information with a field ECO_NUM representing ecoregions Ids. If ecoReg=NULL the function will use a shapefile
#'  provided for your use after run GetDatasets()
#'
#' @return This function returns a data frame summarizing the in-situ gap analysis scores:
#'
#' \tabular{lcc}{
#' species \tab Species name \cr
#' SRSin \tab In-situ sample representativeness score \cr
#' GRSin \tab In-situ germplasm representativeness score \cr
#' ERSin \tab In-situ environmental representativeness score \cr
#' FCSin \tab In-situ final conservation score \cr
#' }
#'
#' @examples
#' ##Obtaining occurrences from example
#' data(CucurbitaData)
#' ##Obtaining species names from the data
#' speciesList <- unique(CucurbitaData$taxon)
#' ##Obtaining raster_list
#' data(CucurbitaRasters)
#' CucurbitaRasters <- raster::unstack(CucurbitaRasters)
#' ##Obtaining protected areas raster
#' data(ProtectedAreas)
#' ##Obtaining ecoregions shapefile
#' data(Ecoregions)
#'
#' #Running all three In situ gap analysis steps using InsituCompile function
#' insituGapMetrics <- InsituCompile(species_list=speciesList,
#'                                        occurrenceData=CucurbitaData,
#'                                        raster_list=CucurbitaRasters,
#'                                        proArea=ProtectedAreas,
#'                                        ecoReg=Ecoregions)
#'
#' @references
#' Ramirez-Villegas, J., Khoury, C., Jarvis, A., Debouck, D. G., & Guarino, L. (2010).
#' A Gap Analysis Methodology for Collecting Crop Genepools: A Case Study with Phaseolus Beans.
#' PLOS ONE, 5(10), e13497. Retrieved from https://doi.org/10.1371/journal.pone.0013497
#'
#' Khoury, C. K., Amariles, D., Soto, J. S., Diaz, M. V., Sotelo, S., Sosa, C. C., … Jarvis, A. (2019).
#' Comprehensiveness of conservation of useful wild plants: An operational indicator for biodiversity
#' and sustainable development targets. Ecological Indicators. https://doi.org/10.1016/j.ecolind.2018.11.016
#'
#' @export

InsituCompile <- function(species_list, occurrenceData, raster_list,proArea,ecoReg){
  # call SRSin
  srsIn_df <- GapAnalysis::SRSin(species_list = species_list,
    occurrenceData = occurrenceData,
     raster_list = raster_list,
    proArea=proArea)
  # call GRSin
  grsIn_df <- GapAnalysis::GRSin(species_list = species_list,
    occurrenceData = occurrenceData,
    raster_list = raster_list,
    proArea=proArea)
  # call ERSin
  ersIn_df <- GapAnalysis::ERSin(species_list = species_list,
    occurrenceData =occurrenceData,
    raster_list = raster_list,
    proArea=proArea,
    ecoReg=ecoReg)

  # call FCSex
  FCSin_df <- GapAnalysis::FCSin(srsIn_df,grsIn_df,ersIn_df)

  # return dataframe from FCSex
  return(FCSin_df)
}
