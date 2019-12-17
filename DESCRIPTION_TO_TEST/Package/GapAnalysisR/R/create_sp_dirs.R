#' @title Creating folder and subfolder structure of gap analysis results for a given species  (Pre-analysis function)
#' @name create_sp_dirs
#' @description Creates the output folders structure for each species to host results obtained by the gap analysis pipeline
#'  This function is used internally in the create_folder_structure function
#' @param species A name species compiled using '_'  to call occurrences files from Workspace/parameter/occurrences folder
#' @param Workspace A forder where the pipeline will be executed
#' @param  run_version The version of the analysis used (e.g 'v1')
#'
#' @return It returns a folder structure with the following subfolder schema:
#' \tabular{lll}{
#'   \tab species name \tab species name as species parameter \cr
#'   \tab └───v1  \tab #Run version \cr
#'   \tab │   ├───gap_analysis \tab # Gap analysis results \cr
#'   \tab │   ├───combined  \tab #Folder where results of fcs_combine.R are saved \cr
#'   \tab │   ├───exsitu  \tab #Folder where Ex-situ indicators results will be saved \cr
#'   \tab │   └───insitu \tab #Folder where In-situ indicators results will be saved \cr
#'   \tab └───redList \tab #Folder where results of eooAoo function will be saved \cr
#' }
#'
#' @examples create_sp_dirs('Cucurbita_digitata',Workspace,'v1')
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
#'    x <- create_sp_dirs(species,Workspace,run_version)
#'    print(paste0(species,' DONE!'))
#' })
#'
#'@references
#'
#' Khoury, C. K., Amariles, D., Soto, J. S., Diaz, M. V., Sotelo, S., Sosa, C. C., … Jarvis, A. (2019).
#' Comprehensiveness of conservation of useful wild plants: An operational indicator for biodiversity
#' and sustainable development targets. Ecological Indicators. https://doi.org/10.1016/j.ecolind.2018.11.016
#'
#' @export

create_sp_dirs <- function(species,gap_dir,run_version) {

  #create species dir
  sp_dir <- paste(gap_dir,"/",species,"/",run_version,sep="")
  if (!file.exists(sp_dir)) {dir.create(sp_dir,recursive=T)}

  #create other directories
  if (!file.exists(paste(sp_dir,"/gap_analysis/combined",sep=""))) {dir.create(paste(sp_dir,"/gap_analysis/combined",sep=""),recursive=T)}
  if (!file.exists(paste(sp_dir,"/gap_analysis/exsitu",sep=""))) {dir.create(paste(sp_dir,"/gap_analysis/exsitu",sep=""),recursive=T)}
  if (!file.exists(paste(sp_dir,"/gap_analsis/insitu",sep=""))) {dir.create(paste(sp_dir,"/gap_analysis/insitu",sep=""),recursive=T)}
  if (!file.exists(paste(sp_dir,"/modeling",sep=""))) {dir.create(paste(sp_dir,"/modeling",sep=""),recursive=T)}#  if (!file.exists(paste(sp_dir,"/modeling/maxent",sep=""))) {dir.create(paste(sp_dir,"/modeling/maxent",sep=""),recursive=T)}
  if (!file.exists(paste(sp_dir,"/redList",sep=""))) {dir.create(paste(sp_dir,"/redList",sep=""),recursive=T)}#  if (!file.exists(paste(sp_dir,"/modeling/maxent",sep=""))) {dir.create(paste(sp_dir,"/modeling/maxent",sep=""),recursive=T)}

    return(species)
  print(paste0("Folders structure created succesfully for ", species))
}
