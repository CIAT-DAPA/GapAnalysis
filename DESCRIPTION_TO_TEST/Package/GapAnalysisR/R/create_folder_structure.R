#' @title Creating folder and subfolder structure for a species list extracted from a CSV file  (Pre-analysis function)
#' @name create_folder_structure
#' @description Creates the output folders structure for each species to host results obtained by the gap analysis pipeline
#'  This function is used internally in the create_folder_structure function when an initial species list is provided
#' @param species_list A list of species to be analyzed when a species list is provided
#' @param dir A forder to run the gap analysis including inputs and outputs
#' @param  run_version The version of the analysis used (e.g 'v1')
#' @param  initial (logical) The function  will create the initial folder structure for the analysis
#' @param  create_sp_dirs (logical) The function  will create folder for each species outputs in /gap_anaylsis
#'
#' @return It returns a folder structure with the following subfolder schema:
#' \tabular{lll}{
#'   \tab workspace \tab species name as species parameter \cr
#'   \tab └───parameters  \tab #Folder where the inputs are hosted \cr
#'   \tab │   ├───raster \tab # Subfolder to add mask, protected areas and ecoregions shapefile \cr
#'   \tab │   ├───occurrences  \tab #Folder where a CSV file is used to filter species accessions occurrences \cr
#'   \tab └───gap_analysis \tab #Folder where results will be saved if create_sp_dirs=T\cr
#' }
#'
#'  Sources used for analysis were obtained from Khoury et al., 2019 using the following download links:
#'
#' \tabular{lll}{
#' \tab Protected area  \tab http://dx.doi.org/10.17632/2jxj4k32m2.1#file-fc8ffd3f-8d79-481b-9ccf-4cf19b8480ec \cr
#' \tab World mask \tab http://dx.doi.org/10.17632/2jxj4k32m2.1#folder-1b61c978-5753-4ab6-bbc7-11f8e4b062cf \cr
#' \tab Terrestrial ecoregions \tab http://maps.tnc.org/files/shp/terr-ecoregions-TNC.zip \cr
#' }
#' @examples create_folder_structure(species_list,dir,run_version,initial=F,create_sp_dirs=T)
#'
#'  dir  <-  'E:/CIAT/workspace/Workspace_test'
#'  run_version  <- 'v1'
#'  species_list <- c('Cucurbita_cordata',
#'   'Cucurbita_digitata',
#'   'Cucurbita_foetidissima',
#'   'Cucurbita_palmata')
#'
#'  x <- create_folder_structure(species,dir,run_version,initial=F,create_sp_dirs=F)
#'
#'@references
#'
#' Khoury, C. K., Amariles, D., Soto, J. S., Diaz, M. V., Sotelo, S., Sosa, C. C., … Jarvis, A. (2019).
#' Comprehensiveness of conservation of useful wild plants: An operational indicator for biodiversity
#' and sustainable development targets. Ecological Indicators. https://doi.org/10.1016/j.ecolind.2018.11.016
#'
#' Khoury, C. K., Amariles, D., Soto, J. S., Diaz, M. V., Sotelo, S., Sosa, C. C., … Wiersema, J. H. (2019).
#' Data for the calculation of an indicator of the comprehensiveness of conservation of useful wild plants.
#' Data in Brief, 22, 90–97. https://doi.org/10.1016/j.dib.2018.11.125
#'
#' @export

create_folder_structure <- function(species_list,dir,run_version,initial,create_sp_dirs) {

   folder_dir <- paste(dir,"/","workspace",sep = "")
  if(initial==T){
  #Creating folder structure according to a given directory
  folder_dir <- paste(dir,"/","workspace",sep = "")
  if (!file.exists(folder_dir)) {dir.create(folder_dir)}

  if (!file.exists(paste(folder_dir,"/parameters",sep=""))) {dir.create(paste(folder_dir,"/parameters",sep=""),recursive=T)}
  # if (!file.exists(paste(folder_dir,"/parameters/models",sep=""))) {dir.create(paste(folder_dir,"/parameters/models",sep=""),recursive=T)}

  model_path_dir <- paste(folder_dir,"/parameters/inputs",sep="")
  if (!file.exists(model_path_dir)) {dir.create(model_path_dir)}

  # lapply(1:length(species_list),function(i){
  #   sp <- species_list[[i]]
  #   if (!file.exists(paste0(model_path_dir,"/",sp))) {dir.create(paste0(model_path_dir,"/",sp),recursive=T)}
  #   return(cat("model path dir for ",as.character(sp),"\n"))
  # })


  if (!file.exists(paste(folder_dir,"/parameters/occurrrences",sep=""))) {dir.create(paste(folder_dir,"/parameters/occurrences",sep=""),recursive=T)}
  if (!file.exists(paste(folder_dir,"/parameters/occurrrences/original",sep=""))) {dir.create(paste(folder_dir,"/parameters/occurrences/original",sep=""),recursive=T)}
  # if (!file.exists(paste(folder_dir,"/parameters/occurrrences/filtered",sep=""))) {dir.create(paste(folder_dir,"/parameters/occurrences/filtered",sep=""),recursive=T)}

  # if (!file.exists(paste(folder_dir,"/parameters/biolayers",sep=""))) {dir.create(paste(folder_dir,"/parameters/biolayers",sep=""),recursive=T)}
  if (!file.exists(paste(folder_dir,"/parameters/raster",sep=""))) {dir.create(paste(folder_dir,"/parameters/raster",sep=""),recursive=T)}

  if (!file.exists(paste(folder_dir,"/gap_analysis",sep=""))) {dir.create(paste(folder_dir,"/gap_analysis",sep=""),recursive=T)}
  # if (!file.exists(paste(folder_dir,"/summaries",sep=""))) {dir.create(paste(folder_dir,"/summaries",sep=""),recursive=T)}
  }
  #Creating subfolder for gap analysis for each species in a species list
  gap_dir <- paste(folder_dir,"/gap_analysis",sep="")

  if(create_sp_dirs==T){
    lapply(1:length(species_list),function(i){
    X <-     create_sp_dirs(species_list[[i]],gap_dir=gap_dir,run_version=run_version)
    return(X)
   })
  } else {
    cat("No species dirs created in /gap_analysis","\n")
}
    return(species_list)
  print(paste0("Folders structure created"))
}
