#' @title Creating a summary HTML document for gap analysis
#' @name summary_HTML
#' @description Calls the summaryHTML rmd file information from all in situ,
#'  ex situ, and EOO AOO summaries and displays the content.
#'  The code also produces in situ and ex situ gap maps which can be written out to disk.
#'
#' @param species_list A species list to calculate metrics.
#' @param occurrenceData A data frame object with the species name, geographical coordinates,
#'  and type of records (G or H) for a given species
#' @param raster_List A list representing the species distribution models for the species list provided
#'  loaded in raster format. This list must match the same order of the species list.
#' @param bufferDistance Geographical distance used to create circular buffers around germplasm.
#'  Default: 50000 that is 50 km around germplasm accessions (CA50)
#' @param proArea A raster file representing protected areas information.
#' If proArea=NULL the funtion will use a protected area raster file provided for your use after run GetDatasets()
#' @param exsituSummary A data frame object result of the functions exsituGapAnalysis or fcs_exsitu
#' @param insituSummary A data frame object result of the functions insituGapAnalysis or fcs_insitu
#' @param fcsSummary A data frame object result of the function fcs_combine
#' @param outputFolder A path to save the HTML file resulting of this function
#' @param writeRasters Boolean field (default=F) to indicate if raster files should be saved
#'
#' @return This function returns a data frame file saved at a specified folder
#' @examples
#' \dontrun{
#' ##Obtaining occurrences from example
#' data(CucurbitaData)
#' ##Obtaining species names from the data
#' Cucurbita_splist <- unique(CucurbitaData$taxon)
#' ##Obtaining raster_list
#' data(CucurbitaRasters)
#' CucurbitaRasters <- raster::unstack(CucurbitaRasters)
#' ##Obtaining protected areas raster
#' data(ProtectedAreas)
#' ##Obtaining ecoregions shapefile
#' data(ecoregions)
#'
#' #Running all three Ex situ gap analysis steps using insituGapAnalysis function
#' FCSex_df <- FCSex(species_list=speciesList,
#'                                       occurrenceData=CucurbitaData,
#'                                       raster_list=CucurbitaRasters,
#'                                       bufferDistance=50000,
#'                                       ecoReg=ecoregions)
#'
#'
#' #Running all three In situ gap analysis steps using insituGapAnalysis function
#' FCSin_df <- FCSin(species_list=speciesList,
#'                                        occurrenceData=CucurbitaData,
#'                                        raster_list=CucurbitaRasters,
#'                                        proArea=ProtectedAreas,
#'                                        ecoReg=ecoregions)
#'
#' fcsCombine <- FCSc_mean(fcsEx = FCSex_df,fcsIn = FCSin_df)
#'
#' summaryHTML_file <- summary_HTML(species_list=Cucurbita_splist,
#'                                  occurrenceData = CucurbitaData,
#'                                  raster_List=CucurbitaRasters,
#'                                  proArea=ProtectedArea,
#'                                  bufferDistance=50000,
#'                                  insituSummary=FCSin_df,
#'                                  exsituSummary=FCSex_df,
#'                                  fcsSummary=fcsCombine,
#'                                  outputFolder=".",
#'                                  writeRasters=F)
#' }
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
#' @importFrom rmarkdown render
#' @importFrom tmap tmap_mode qtm
#' @importFrom raster raster extend writeRaster crop extent
#' @importFrom sp coordinates proj4string CRS
#' @importFrom dplyr select filter

summary_HTML <- function(species_list,occurrenceData, raster_List,  proArea,bufferDistance,
                        #countsSummary,
                        insituSummary, exsituSummary,
                        fcsSummary,  outputFolder,
                        writeRasters){

out_dir <- system.file(package = "GapAnalysis")

if(!file.exists(paste0(out_dir,"/","preloaded_data","/","summaryHTML.Rmd"))){
  stop("Rmd file is not available yet. Please run the function preparingDatasets() and try again")
} else {

rmarkdown::render(input = paste0(out_dir,"/","preloaded_data","/","summaryHTML.Rmd"),
      output_dir = outputFolder,
      output_file  = "SummaryReport.html"
       )

  }
}
