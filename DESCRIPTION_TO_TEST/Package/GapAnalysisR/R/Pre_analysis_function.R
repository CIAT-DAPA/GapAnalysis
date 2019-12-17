#' @title Run the first steps for the gap analysis
#' @name Pre_analysis_function
#' @description Creates initial steps for the gap analysis as folder structure, species folder results and species list names
#'
#' @param dir A forder to run the gap analysis including inputs and outputs
#' @param run_version The version of the analysis used (e.g 'v1')
#' @param Workspace A forder where the pipeline will be executed
#' @param species_csv CSV file name located at /Workspace/parameters/input to be splitted
#' @return This function run the first steps for the GapAnalysisR pipeline
#'
#' @examples Pre_analysis_function(dir,run_version,Workspace,buff_dist,pre_analysis,insitu,exsitu,eooAoo)
#'
#'  dir  <-  'E:/CIAT/workspace/Workspace_test'
#'  run_version  <- 'v1'
#'  species_csv <- 'Cucurbita_CWR_2019_09_30.csv'
#'  Workspace  <-  'E:/CIAT/workspace/Workspace_test/workspace'
#'
#' x <- Pre_analysis_function(dir,species_csv,run_version,Workspace)
#'
#'@references
#'
#' Khoury, C. K., Amariles, D., Soto, J. S., Diaz, M. V., Sotelo, S., Sosa, C. C., … Jarvis, A. (2019).
#' Comprehensiveness of conservation of useful wild plants: An operational indicator for biodiversity
#' and sustainable development targets. Ecological Indicators. https://doi.org/10.1016/j.ecolind.2018.11.016
#'
#' @export

Pre_analysis_function <- function(dir,species_csv,run_version){
  x <- create_folder_structure(species_list,dir,run_version,initial=T,create_sp_dirs=F)
  state <-"0. Folder structure created"
  x <- get_sp_names(Workspace,species_csv); species_list <- x
  state <-"1. Species name acquired"
  x <- create_folder_structure(species_list,dir,run_version,initial=F,create_sp_dirs=T)
  state <-"1.1 Species folders created"
  x <- clean_records(Workspace,species_csv,species_list,run_version)
  state <-"2. Splitting records in CSV files"

  return(state)
}
