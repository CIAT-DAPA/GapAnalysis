#' @title Germplasm representativeness score estimation (Ex-situ conservation indicators).
#' @name grs_exsitu
#' @description Performs an estimation of germplasm representativeness score for ex-situ gap analysis (GRSex) using Ramirez-Villegas et al., (2010) methodology
#' This function uses a germplasm buffer raster file (e.g. CA50) and a thresholded species distribution model.
#'  \deqn{GRSex = min(100,(Germplasm buffer area/species distribution in protected areas)*100)}
#'
#' @param species A name species compiled using '_'  to call the raster files (SDM and germplasm buffer)
#'  from Workspace/parameters/inputs folder and occurrences files from Workspace/parameter/occurrences folder
#' @param Workspace A forder where the pipeline will be executed
#' @param  run_version The version of the analysis used (e.g 'v1')
#'
#' @return It returns a data frame file saved at gap_analysis folder with four columns:
#'
#' \tabular{lcc}{
#'  ID \tab Species name \cr
#'  SPP_AREA_km2 \tab Area occupied by the species using as input a SDM thresholded file in tiff format \cr
#'  G_AREA_km2 \tab Area occupied by the germplasm accessions in a species distribution model \cr
#'  GRS \tab GRSex result \cr
#' }
#'
#' @examples grs_exsitu('Cucurbita_digitata',Workspace,'v1')
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
#'    x <- grs_exsitu(species,Workspace,run_version)
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
# This function calculates the GRSex. It loads occurrences if they exist, then
# loads the presence/absence surface, creates the G buffer (i.e. CA50) and finally
# outputs the GRS and areas in a data.frame (which is written into a file).
# @param (string) species: species ID
# @param (logical) debug: whether to save or not the intermediate raster outputs
# @return (data.frame): This function returns a data frame with GRS and areas of G buffer (i.e. CA50)
#                       and of the presence/absence surface.

grs_exsitu <- function(species,Workspace,run_version) {

suppressMessages(require(rgdal))
suppressMessages(require(raster))

#importFrom("methods", "as")
#importFrom("stats", "complete.cases", "filter", "median")
#importFrom("utils", "data", "memory.limit", "read.csv", "write.csv")


folderin_raw <- paste0(Workspace,"/","parameters","/","occurrences")
sp_dir <- paste0(Workspace,"/gap_analysis","/",species,"/",run_version)
model_input_dir <- paste0(Workspace,"/parameters/inputs")
sp_counts <- read.csv(paste0(sp_dir,"/counts.csv"))

  if(!file.exists(paste0(model_input_dir,"/",species,"_ca50_G_narea.tif"))){
    grs <- 0
    gBufferRas_area <- 0
    pa_spp_area <- NA
  }else{

  # load in ga50 and model outputs
  gBufferRas <- raster::raster(paste0(model_input_dir,"/",species,"_ca50_G_narea.tif"))
  cell_size<-raster::area(gBufferRas, na.rm=TRUE, weights=FALSE)
  cell_size<-cell_size[!is.na(cell_size)]
  gBufferRas_area<-length(cell_size)*median(cell_size)

  pa_spp <<- raster::raster(paste0(model_input_dir,"/",species,"_spdist_thrsld.tif"))
  cell_size<- raster::area(pa_spp, na.rm=TRUE, weights=FALSE)
  cell_size<- cell_size[!is.na(cell_size)]
  pa_spp_area <<-length(cell_size)*median(cell_size)

  #calculate GRS
  grs <- min(c(100, gBufferRas_area/pa_spp_area*100))
    }

#create data.frame with output
out_df <- data.frame(ID=species, SPP_AREA_km2=pa_spp_area, G_AREA_km2=gBufferRas_area, GRS=grs)
write.csv(out_df,paste(sp_dir,"/gap_analysis/exsitu/grs_result.csv",sep=""),row.names=F)

  #return object
  return(out_df)
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


# lapply(1:length(species_list),function(i){
#   species <- species_list[[i]]
#   x <- grs_exsitu(species,Workspace,run_version)
#   cat(paste0(species," DONE!"),"\n")
# })
