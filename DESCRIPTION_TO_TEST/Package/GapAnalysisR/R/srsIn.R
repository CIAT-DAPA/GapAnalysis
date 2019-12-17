#' @title Sample representativeness score estimation (In-situ conservation indicators).
#' @name srs_insitu
#' @description Performs an estimation of sample representativeness score for in-situ gap analysis (SRSin) using Khoury et al., (2019) methodology
#' This function uses counts from herbarium an germplasm accessions and calculate the SRS in-situ score as:
#'  \deqn{SRSin = Number of germplasm accessions in protected areas / Number of herbarium accessions in protected areas}
#'
#' @param species A name species compiled using '_'  to call occurrences files from Workspace/parameter/occurrences folder
#' @param Workspace A forder where the pipeline will be executed
#' @param  run_version The version of the analysis used (e.g 'v1')
#'
#' @return It returns a data frame file saved at gap_analysis folder with eight fields:
#'
#' \tabular{lcc}{
#'  ID \tab Species name \cr
#'  NTOTAL \tab Number of records available for a given species in protected areas \cr
#'  NTOTAL_COORDS \tab Number of records with geographical coordinates available for a given species in protected areas  \cr
#'  NG \tab Number of germplasm accessions available for a given species in protected areas  \cr
#'  NG_COORDS \tab Number of germplasm accessions with geographical coordinates Savailable for a given species in protected areas  \cr
#'  NH \tab Number of herbarium accessions available for a given specie in protected areas s \cr
#'  NH_COORDS \tab Number of herbarium accessions with geographical coordinates Savailable for a given species in protected areas  \cr
#'  SRS \tab SRSin score calculated from a CSV file summarizing the number of records for a given species in protected areas  \cr
#' }
#'
#' @examples srs_insitu('Cucurbita_digitata',Workspace,'v1')
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
#'    x <- srs_insitu(species,Workspace,run_version)
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

###
# Calculate the proportion of points that fall within a protected areas. Insitu SRS
# 20191002
# carver.dan1@gmail.com
###
srs_insitu <- function(species,Workspace,run_version){

  #importFrom("methods", "as")
  #importFrom("stats", "complete.cases", "filter", "median")
  #importFrom("utils", "data", "memory.limit", "read.csv", "write.csv")

  par_dir <- paste0(Workspace,"/","parameters")
  folderin_raw <- paste0(Workspace,"/","parameters","/","occurrences")
  sp_dir <- paste0(Workspace,"/gap_analysis","/",species,"/",run_version)
  model_input_dir <- paste0(Workspace,"/parameters/inputs")
  occ_dir <- paste0(Workspace,"/","parameters","/","occurrences")

  proArea <- raster::raster(paste0(par_dir,"/raster/","wdpa_reclass.tif"))
  cleanPoints <- read.csv(paste0(occ_dir,'/original/',species,'_original.csv'),header=T)[,1:2]#[,c("lon","lat")]
  colnames(cleanPoints) <- c("lon","lat")
  sp::coordinates(cleanPoints) <- ~lon + lat

  totalNum <- nrow(cleanPoints@coords)

  # set coodinate systems equal
  crs(cleanPoints) = raster::crs(proArea)
  # run a extract to values and select all data. Test for NA, then sum true values for total number of points in protected
  #areas
  protectPoints <- sum(!is.na(raster::extract(x = proArea,y = cleanPoints)))

  #define SRS
  if(protectPoints >= 0 ){
    srsInsitu <- 100 *(protectPoints/totalNum)
  }else{
    srsInsitu <- 0
  }

    #create data.frame with output
  out_df <- data.frame(ID=species,
                       NTOTAL=totalNum,
                       ProTotal = protectPoints,
                       SRS=srsInsitu)
  write.csv(out_df,paste0(sp_dir,"/gap_analysis/insitu/srs_result.csv"),row.names=F)

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

#lapply(1:length(species_list),function(i){
#  species <- species_list[[i]]
# x <-  srs_insitu(species_list[[i]],Workspace,run_version)
# cat(paste0(species," DONE!"),"\n")
# })
