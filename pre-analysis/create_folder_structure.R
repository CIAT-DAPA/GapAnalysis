create_folder_structure <- function(species_list,dir,run_version) {
  #Creating folder structure according to a given directory 
  folder_dir <- paste(dir,"/","workspace",sep = "")
  if (!file.exists(folder_dir)) {dir.create(folder_dir)}
  
  if (!file.exists(paste(folder_dir,"/parameters",sep=""))) {dir.create(paste(folder_dir,"/parameters",sep=""),recursive=T)}
  if (!file.exists(paste(folder_dir,"/parameters/models",sep=""))) {dir.create(paste(folder_dir,"/parameters/models",sep=""),recursive=T)}

  model_path_dir <- paste(folder_dir,"/parameters/models",sep="")
  
  lapply(1:length(species_list),function(i){
    sp <- species_list[[i]]
    if (!file.exists(paste0(model_path_dir,"/",sp))) {dir.create(paste0(model_path_dir,"/",sp),recursive=T)}
    return(cat("model path dir for ",as.character(sp),"\n"))
  }) 
  
  
  if (!file.exists(paste(folder_dir,"/parameters/occurrrences",sep=""))) {dir.create(paste(folder_dir,"/parameters/occurrences",sep=""),recursive=T)}
  if (!file.exists(paste(folder_dir,"/parameters/occurrrences/original",sep=""))) {dir.create(paste(folder_dir,"/parameters/occurrences/original",sep=""),recursive=T)}
  if (!file.exists(paste(folder_dir,"/parameters/occurrrences/filtered",sep=""))) {dir.create(paste(folder_dir,"/parameters/occurrences/filtered",sep=""),recursive=T)}
  
  if (!file.exists(paste(folder_dir,"/parameters/biolayers",sep=""))) {dir.create(paste(folder_dir,"/parameters/biolayers",sep=""),recursive=T)}
  if (!file.exists(paste(folder_dir,"/parameters/raster",sep=""))) {dir.create(paste(folder_dir,"/parameters/raster",sep=""),recursive=T)}
  
  if (!file.exists(paste(folder_dir,"/gap_analysis",sep=""))) {dir.create(paste(folder_dir,"/gap_analysis",sep=""),recursive=T)}
  if (!file.exists(paste(folder_dir,"/summaries",sep=""))) {dir.create(paste(folder_dir,"/summaries",sep=""),recursive=T)}

  #Creating subfolder for gap analysis for each species in a species list
  gap_dir <- paste(folder_dir,"/gap_analysis",sep="")
    lapply(1:length(species_list),function(i){
    X <-     create_sp_dirs(species_list[[i]],gap_dir=gap_dir,run_version=run_version)
    return(X)
   }) 
  
    return(species_list)
  print(paste0("Folders structure created"))
}
