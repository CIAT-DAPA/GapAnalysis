#' @title Estimating priority areas for further collecting (Ex-situ conservation indicators).
#' @name gap_map_exsitu
#' @description Calculates a raster file obtaining collecting areas from the species distribution to collect germplasm accessions
#'  to enrich germplasm banks
#' @param species A name species compiled using '_'  to call occurrences files from Workspace/parameter/occurrences folder
#' @param Workspace A forder where the pipeline will be executed
#' @param  run_version The version of the analysis used (e.g 'v1')
#'
#' @return It returns a raster file in tiff format using a species distribution model and germplasm buffer raster files in
#'  tiff format read from /Workspace/parameters/inputs
#'
#' @examples gap_map_exsitu('Cucurbita_digitata',Workspace,'v1')
#'
#' Workspace  <-  'E:/CIAT/workspace/Workspace_test/workspace'
#' run_version  <- 'v1'
#' species_list <- c('Cucurbita_cordata',
#'  'Cucurbita_digitata',
#'  'Cucurbita_foetidissima',
#'  'Cucurbita_palmata')
#'
#'  run_version <-'v1'
#
#' lapply(1:length(species_list),function(i){
#'    species <- species_list[[i]]
#'    x <- gap_map_exsitu(species,Workspace,run_version)
#'    print(paste0(species,' DONE!'))
#' })
#'
#'@references
#'
#'Ramírez-Villegas, J., Khoury, C., Jarvis, A., Debouck, D. G., & Guarino, L. (2010).
#'A Gap Analysis Methodology for Collecting Crop Genepools: A Case Study with Phaseolus Beans.
#'PLOS ONE, 5(10), e13497. Retrieved from https://doi.org/10.1371/journal.pone.0013497
#'
#' Khoury, C. K., Amariles, D., Soto, J. S., Diaz, M. V., Sotelo, S., Sosa, C. C., … Jarvis, A. (2019).
#' Comprehensiveness of conservation of useful wild plants: An operational indicator for biodiversity
#' and sustainable development targets. Ecological Indicators. https://doi.org/10.1016/j.ecolind.2018.11.016
#'
#' @export

##########################################   Start Functions    ###############################################
# This function calculates a gap maps It loads sp_thrshld, and germplasm buffer
# calculates FCS. It saves output in summary.csv
# @param (string) species: species ID
# @return (data.frame): This function returns a data frame with ID, SRS, GRS, ERS, FCS
#                       for a given species.
gap_map_exsitu <- function(species,Workspace,run_version) {

suppressMessages(require(rgdal))
suppressMessages(require(raster))

#importFrom("methods", "as")
#importFrom("stats", "complete.cases", "filter", "median")
#importFrom("utils", "data", "memory.limit", "read.csv", "write.csv")

sp_dir <- paste0(Workspace,"/gap_analysis","/",species,"/",run_version)
model_input_dir <- paste0(Workspace,"/parameters/inputs")
par_dir <- paste0(Workspace,"/","parameters")

if(!file.exists(paste0(sp_dir,"/gap_analysis/exsitu/gap_map.tif"))){
  if(file.exists(paste0(par_dir,"/","inputs","/",species,"_","ca50_G_narea.tif"))){
    gBuffer <- raster::raster(paste0(par_dir,"/","inputs","/",species,"_","ca50_G_narea.tif"))
  } else {
    gBuffer <- NULL
  }

  pa_spp <- raster::raster(paste0(model_input_dir,"/",species,"_","spdist_thrsld.tif"))
  pa_spp[which(pa_spp[] == 0)] <- NA

  if(!is.null(gBuffer)){
    gap_map <- pa_spp + gBuffer
    gap_map[which(gap_map[]==2)] <- 0
  } else {
    gap_map <- pa_spp
  }
  raster::writeRaster(gap_map,paste0(sp_dir,"/gap_analysis/exsitu/gap_map.tif"))
} else {
  gap_map <- raster::raster(paste0(sp_dir,"/gap_analysis/exsitu/gap_map.tif"))
}
  return(gap_map)
}

#Workspace = "E:/CIAT/workspace/Workspace_test/workspace"
#run_version="v1"
#species_list <- c(
#  "Cucurbita_cordata",
#  "Cucurbita_digitata",
#  "Cucurbita_foetidissima",
#  "Cucurbita_palmata"
#)
#run_version <-"v1"


#lapply(1:length(species_list),function(i){
#  species <- species_list[[i]]
#  x <- gap_map_exsitu(species,Workspace,run_version)
#  cat(paste0(species," DONE!"),"\n")
#})
