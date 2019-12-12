create_sp_dirs <- function(species,gap_dir,run_version) {
  #load config
  # config(dirs=T)
  
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
