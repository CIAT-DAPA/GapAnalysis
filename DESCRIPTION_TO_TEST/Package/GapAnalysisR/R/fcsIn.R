#' @title Final in-situ conservation score estimation (In-situ conservation indicators).
#' @name fcs_insitu
#' @description Concatenates the SRSin, GRSin, and ERSin values in a unique file, Then calculates a final priority scores as
#'  the average of the SRSin, GRSin, and ERSin values representing how well is conserved a species comparing
#'  its species distribution, germplasm buffers and occurrences files for protected areas
#'
#'  \deqn{FCSin = mean(SRSin,GRSin,ERSin)}
#'
#' @param species A name species compiled using '_'  to call occurrences files from Workspace/parameter/occurrences folder
#' @param Workspace A forder where the pipeline will be executed
#' @param  run_version The version of the analysis used (e.g 'v1')
#'
#' @return It returns a data frame file saved at gap_analysis folder with five fields:
#'
#' \tabular{lcc}{
#'  ID \tab Species name \cr
#'  SRS \tab In-situ sample representativeness score \cr
#'  GRS \tab In-situ germplasm representativeness score \cr
#'  ERS \tab In-situ environmental representativeness score  \cr
#'  FCS \tab In-situ final conservation score  \cr
#' }
#'
#' @examples fcs_insitu('Cucurbita_digitata',Workspace,'v1')
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
#'    x <- fcs_insitu(species,Workspace,run_version)
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

fcs_insitu <- function(species,Workspace,run_version) {

  #importFrom("methods", "as")
  #importFrom("stats", "complete.cases", "filter", "median")
  #importFrom("utils", "data", "memory.limit", "read.csv", "write.csv")

  sp_dir <- paste0(Workspace,"/gap_analysis","/",species,"/",run_version)

  #load GRS, and ERS file
  sp_srs <- read.csv(paste0(sp_dir,"/gap_analysis/insitu/srs_result.csv"))
  sp_grs <- read.csv(paste0(sp_dir,"/gap_analysis/insitu/grs_result.csv"))
  sp_ers <- read.csv(paste0(sp_dir,"/gap_analysis/insitu/ers_result.csv"))

  sp_fcs <- mean(c(sp_srs$SRS,sp_grs$GRS,sp_ers$ERS), na.rm=T)

  #create data.frame with output
  out_df <- data.frame(ID=species,SRS=sp_srs, GRS=sp_grs$GRS, ERS=sp_ers$ERS, FCS=sp_fcs)
  write.csv(out_df,paste(sp_dir,"/gap_analysis/insitu/summary.csv",sep=""),row.names=F)

  #return object
  return(out_df)
}

# Workspace = "E:/CIAT/workspace/Workspace_test/workspace"
# run_version="v1"
# species_list <- c(
#   "Cucurbita_cordata",
#   "Cucurbita_digitata",
#   "Cucurbita_foetidissima",
#   "Cucurbita_palmata"
# )
# run_version <-"v1"
#
# lapply(1:length(species_list),function(i){
#   species <- species_list[[i]]
#   x <- fcs_insitu(species,Workspace,run_version)
#   cat(paste0(species," DONE!"),"\n")
# })
