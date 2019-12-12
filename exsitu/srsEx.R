srs_exsitu <- function(species,Workspace,run_version) {
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
