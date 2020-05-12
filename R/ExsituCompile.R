#' @title Ex-situ gap analysis calculation (Ex-situ conservation)
#' @name ExsituCompile
#' @description This function allows calculate all the three Ex situ gap analysis scores
#'   in one unique function returning a final conservation score summary table
#'
#' @param species_list A species list to calculate metrics.
#' @param occurrenceData A data frame object with the species name, geographical coordinates,
#'   and type of records (G or H) for a given species
#' @param raster_list A list representing the species distribution models for the species list provided
#'  loaded in raster format. This list must match the same order of the species list.
#' @param bufferDistance Geographical distance used to create circular buffers around germplasm.
#'  Default: 50000 that is 50 km around germplasm accessions (CA50)
#' @param ecoReg A shapefile representing ecoregions information with a field ECO_NUM representing ecoregions Ids.
#'  If ecoReg=NULL the funtion will use a shapefile provided for your use after run GetDatasets()
#'
#' @return This function returns a data frame summarizing the ex-situ gap analysis scores:
#'
#' \tabular{lcc}{
#' species \tab Species name \cr
#' SRSex \tab Ex-situ sample representativeness score \cr
#' GRSex \tab Ex-situ germplasm representativeness score \cr
#' ERSex \tab Ex-situ environmental representativeness score \cr
#' FCSex \tab Ex-situ final conservation score \cr
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
#' ##Obtaining ecoregions shapefile
#' data(ecoregions)
#'
#' #Running all three Ex situ gap analysis steps using ExsituCompile function
#' ExsituGapMetrics <- ExsituCompile(species_list=speciesList,
#'                                       occurrenceData=CucurbitaData,
#'                                       raster_list=CucurbitaRasters,
#'                                       bufferDistance=50000,
#'                                       ecoReg=ecoregions)
#'
#'@references
#' Ramirez-Villegas, J., Khoury, C., Jarvis, A., Debouck, D. G., & Guarino, L. (2010).
#' A Gap Analysis Methodology for Collecting Crop Genepools: A Case Study with Phaseolus Beans.
#' PLOS ONE, 5(10), e13497. Retrieved from https://doi.org/10.1371/journal.pone.0013497
#'
#' Khoury, C. K., Amariles, D., Soto, J. S., Diaz, M. V., Sotelo, S., Sosa, C. C., … Jarvis, A. (2019).
#' Comprehensiveness of conservation of useful wild plants: An operational indicator for biodiversity
#' and sustainable development targets. Ecological Indicators. https://doi.org/10.1016/j.ecolind.2018.11.016
#'
#' @export
#' @keywords internal
#'
ExsituCompile <- function(species_list, occurrenceData, raster_list, bufferDistance,ecoReg){
  SRS_ex_df <- NULL
  GRS_ex_df <- NULL
  ERS_ex_df <- NULL
  FCSex_list <- NULL

  # call SRSex
  SRS_ex_df <- SRSex(species_list = species_list,
                                    occurrenceData = occurrenceData)
  # call GRSex
  GRS_ex_df <- GRSex(occurrenceData = occurrenceData,
    species_list = species_list,
    raster_list = raster_list,
    bufferDistance = bufferDistance)
  # call ERSex
  ERS_ex_df <- ERSex(species_list = species_list,
    occurrenceData = occurrenceData,
    raster_list = raster_list,
    bufferDistance = bufferDistance,
    ecoReg=ecoReg)

  # join the dataframes base on species
  FCS_ex_df <- dplyr::left_join(SRS_ex_df, GRS_ex_df, by ="species")
  FCS_ex_df <- dplyr::left_join(FCS_ex_df, ERS_ex_df, by = "species") #%>%
  #    dplyr::select("species","SRSex", "GRSex", "ERSex")
  # calculate the mean value for each row to determine fcs per species
  for(i in seq_len(nrow(FCS_ex_df))){
    FCS_ex_df$FCSex[i] <- base::mean(c(FCS_ex_df$SRSex[i], FCS_ex_df$GRSex[i], FCS_ex_df$ERSex[i]))
  }

  # call FCSex
FCSex_list <- list(FCS_ex=FCS_ex_df)
  # return dataframe from FCSex
  return(FCSex_list)
}
