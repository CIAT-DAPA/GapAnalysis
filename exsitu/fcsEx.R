fcs_exsitu <- function(species,Workspace,run_version) {

  sp_dir <- paste0(Workspace,"/gap_analysis","/",species,"/",run_version)
  
  #load SRS, GRS, and ERS file
  sp_srs <- read.csv(paste0(sp_dir,"/gap_analysis/exsitu/srs_result.csv"))
  sp_grs <- read.csv(paste0(sp_dir,"/gap_analysis/exsitu/grs_result.csv"))
  sp_ers <- read.csv(paste0(sp_dir,"/gap_analysis/exsitu/ers_result.csv"))

  sp_fcs <- mean(c(sp_srs$SRS,sp_grs$GRS,sp_ers$ERS), na.rm=T)
  
  #create data.frame with output
  out_df <- data.frame(ID=species, SRS=sp_srs$SRS, GRS=sp_grs$GRS, ERS=sp_ers$ERS, FCS=sp_fcs)
  write.csv(out_df,paste(sp_dir,"/gap_analysis/exsitu/summary.csv",sep=""),row.names=F)
  
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
# 
# lapply(1:length(species_list),function(i){
#   species <- species_list[[i]]
#   x <- fcs_exsitu(species,Workspace,run_version)
#   cat(paste0(species," DONE!"),"\n")
# })

