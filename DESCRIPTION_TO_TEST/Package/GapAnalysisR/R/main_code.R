#' @title Run a combined gap analysis combining all the functions of GapAnalysR package
#' @name Integrate_gap_analysis_function
#' @description Allows calculate all indicators provided by GapAnalysisR R package.
#'  Please use the Pre_analysis_function function before run this pipeline
#'
#' @param dir A forder to run the gap analysis including inputs and outputs
#' @param species Species name provided by user or the get_sp_names function
#' @param run_version The version of the analysis used (e.g 'v1')
#' @param Workspace A forder where the pipeline will be executed
#' @param buff_dist Distance around germplasm accessions geographical coordinates in km^2
#' @param species_csv CSV file name located at /Workspace/parameters/input to be splitted
#' @param pre_analysis (logical) This option allows to run the set of pre-analysis functions:
#' \tabular{lll}{
#'  create_buffers \tab  \cr
#' }
#' @param insitu (logical) This option allows to calculate In-situ indicators:
#'
#' \tabular{lll}{
#'  srs_insitu \tab  \cr
#'  grs_insitu \tab  \cr
#'  ers_insitu \tab  \cr
#'  fcs_insitu \tab  \cr
#' }
#' @param exsitu (logical) This option allows to calculate Ex-situ indicators:
#' \tabular{lll}{
#'  srs_exsitu \tab  \cr
#'  grs_exsitu \tab  \cr
#'  ers_exsitu \tab  \cr
#'  fcs_exsitu \tab  \cr
#'  gap_map_exsitu \tab  \cr
#' }
#' @param eooAoo (logical) This option allows to calculate EOO and AOO to get IUCN conservation status
#' @return This function run the complete GapAnalysisR pipeline in one unique function
#' @examples Integrate_gap_analysis_function(dir,run_version,Workspace,buff_dist,pre_analysis,insitu,exsitu,eooAoo)
#'
#'  dir  <-  'E:/CIAT/workspace/Workspace_test'
#'  run_version  <- 'v1'
#'  species_csv <- 'Cucurbita_CWR_2019_09_30.csv'
#'  Workspace  <-  'E:/CIAT/workspace/Workspace_test/workspace'
#'  buff_dist <- 50000 #50km radius
#'  pre_analysis <- T
#'  insitu <- T
#'  exsitu <- T
#'  eooAoo <- T
#'
#'  Integrate_gap_analysis_function(dir,run_version,Workspace,species,buff_dist,species_csv,pre_analysis,insitu,exsitu,eooAoo)
#'  #INCLUDE INPUT FILES BEFORE RUN THE NEXT FUNCTION
#'
#'  species_list <- get_sp_names(Workspace,species_csv)
#'
#'  lapply(1:length(species_list),function(i){
#'   species <- species_list[[i]]
#'   x <- Integrate_gap_analysis_function(dir,run_version,Workspace,species,buff_dist,species_csv,pre_analysis,insitu,exsitu,eooAoo)
#'   cat(paste0(species,"| status:",x),"\n")
#'  })
#'
#'@references
#'
#' Khoury, C. K., Amariles, D., Soto, J. S., Diaz, M. V., Sotelo, S., Sosa, C. C., … Jarvis, A. (2019).
#' Comprehensiveness of conservation of useful wild plants: An operational indicator for biodiversity
#' and sustainable development targets. Ecological Indicators. https://doi.org/10.1016/j.ecolind.2018.11.016
#'
#' @export

Integrate_gap_analysis_function <- function(dir,run_version,Workspace,species,buff_dist,species_csv,pre_analysis,insitu,exsitu,eooAoo){

###PRE-ANALYSIS
  if(pre_analysis==T){
  x <- create_buffers(species,Workspace,buff_dist,run_version)
  state <-"3. Creating germplasm buffers"
  } else {
  cat("No Pre-analysis functions used","\n")
    state <-"No Pre-analysis functions used"
  }
  if(exsitu==T){
###EX SITU ANALYSIS
  x <- srs_exsitu(species,Workspace,run_version)
  state <-"4. Ex-situ srs calculated"
  x <- grs_exsitu(species,Workspace,run_version)
  state <-"5. Ex-situ grs calculated"
  x <- ers_exsitu(species,Workspace,run_version)
  state <-"6. Ex-situ ers calculated"
  x <- fcs_exsitu(species,Workspace,run_version)
  state <-"7. Ex-situ fcs calculated"
  x <- gap_map_exsitu(species,Workspace,run_version)
  state <-"8. Ex-situ gap map calculated"
  } else {
    cat("No ex-situ indicators to be calculated","\n")
    state <-"No ex-situ indicators to be calculated"
  }
  if(insitu==T){
  ###IN SITU ANALYSIS
  x <- srs_insitu(species,Workspace,run_version)
  state <-"9. In-situ srs calculated"
  x <- insitu_grs(species,Workspace,run_version)
  state <-"10. IN-situ grs calculated"
  x <- ers_insitu(species,Workspace,run_version)
  state <-"11. In-situ ers calculated"
  x <- fcs_insitu(species,Workspace,run_version)
  state <-"12. In-situ fcs calculated"

    } else {
   cat("No in-situ indicators to be calculated","\n")
      x <- "No in-situ indicators to be calculated"
  }
  if(insitu & exsitu==T){
  ###COMBINED ANALYSIS
  x <- fcs_combine(species,Workspace,run_version)
  state <-"13. combined final conservation score calculated"
  } else {
   cat("No in-situ and ex-situ indicators joined","\n")
    x <- "No in-situ and ex-situ indicators joined"
  }

  if(eooAoo==T){
  ###EOO-AOO ANALYSIS
  x <- eooAoo(species,Workspace,run_version)
  state <-"13. EOO and AOO calculated"
  } else {
    cat("No eooAoo calculated","\n")
    state <-"No EOO and AOO calculated"
  }
return(state)
}


 # Integrate_gap_analysis_function(dir,run_version,Workspace,species,buff_dist,species_csv,pre_analysis,insitu,exsitu,eooAoo)
 # #INCLUDE INPUT FILES BEFORE RUN THE NEXT FUNCTION
 #
 # species_list <- get_sp_names(Workspace,species_csv)
 #
 # lapply(1:length(species_list),function(i){
 #  species <- species_list[[i]]
 #  x <- Integrate_gap_analysis_function(dir,run_version,Workspace,species,buff_dist,species_csv,pre_analysis,insitu,exsitu,eooAoo)
 #  cat(paste0(species,"| status:",x),"\n")
 # })
