#' @title Sample representativeness score estimation (Ex-situ conservation indicators).
#' @name srs_exsitu
#' @description Performs an estimation of sample representativeness score for ex-situ gap analysis (SRSex) using Ramirez-Villegas et al., (2010) methodology
#' This function uses counts from herbarium an germplasm accessions and calculate the SRS ex-situ score as:
#'  \deqn{SRSex = Number of germplasm accessions / Number of herbarium accessions}
#'
#' @param species A name species compiled using '_'  to call occurrences files from Workspace/parameter/occurrences folder
#' @param Workspace A forder where the pipeline will be executed
#' @param  run_version The version of the analysis used (e.g 'v1')
#'
#' @return It returns a data frame file saved at gap_analysis folder with eight columns:
#'
#' \tabular{lcc}{
#'  ID \tab Species name \cr
#'  NTOTAL \tab Number of records available for a given species \cr
#'  NTOTAL_COORDS \tab Number of records with geographical coordinates available for a given species \cr
#'  NG \tab Number of germplasm accessions available for a given species \cr
#'  NG_COORDS \tab Number of germplasm accessions with geographical coordinates Savailable for a given species \cr
#'  NH \tab Number of herbarium accessions available for a given species \cr
#'  NH_COORDS \tab Number of herbarium accessions with geographical coordinates Savailable for a given species \cr
#'  SRS \tab SRSex score calculated from a CSV file summarizing the number of records for a given species \cr
#' }
#'
#' @examples srs_exsitu('Cucurbita_digitata',Workspace,'v1')
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
#'    x <- srs_exsitu(species,Workspace,run_version)
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
# This function calculates the ex-situ SRS. It loads counts.csv and computes SRS
# @param (string) species: species ID
# @return (data.frame): This function returns a data frame with SRS, and numbers
#                       of G, H, and total samples, with and without coordinates.


srs_exsitu <- function(species,Workspace,run_version) {

  #importFrom("methods", "as")
  #importFrom("stats", "complete.cases", "filter", "median")
  #importFrom("utils", "data", "memory.limit", "read.csv", "write.csv")

  folderin_raw <- paste0(Workspace,"/","parameters","/","occurrences")
  sp_dir <- paste0(Workspace,"/gap_analysis","/",species,"/",run_version)
  sp_counts <<- read.csv(paste0(sp_dir,"/counts.csv"))

  if(sp_counts$totalGRecords >= 1 & sp_counts$totalHRecords == 0){
    srs <-100
  }

  #### this works for full distributions
  if (sp_counts$totalGRecords == 0 & sp_counts$totalHRecords ==0) {
    srs <- 0
  } else {
    srs <- min(c(100,sp_counts$totalGRecords/sp_counts$totalHRecords*100))
  }


  #create data.frame with output
  out_df <- data.frame(ID=species,
                       NTOTAL=sp_counts$totalRecords,
                       NTOTAL_COORDS=sp_counts$totalUseful,
                       NG= sp_counts$totalGRecords,
                       NG_COORDS=sp_counts$totalGUseful,
                       NH=sp_counts$totalHRecords,
                       NH_COORDS=sp_counts$totalHUseful,
                       SRS=srs)
  write.csv(out_df,paste0(sp_dir,"/gap_analysis/exsitu/srs_result.csv"),row.names=F)


  #return object
  return(out_df)
}

#
# Workspace = "E:/CIAT/workspace/Workspace_test/workspace"
# run_version="v1"
# species_list <- c(
#   "Cucurbita_cordata",
#   "Cucurbita_digitata",
#   "Cucurbita_foetidissima",
#   "Cucurbita_palmata"
# )
#
# lapply(1:length(species_list),function(i){
# species <- species_list[[i]]
#  x <-  srs_exsitu(species_list[[i]],Workspace,run_version)
#   cat(paste0(species," DONE!"),"\n")
# })
