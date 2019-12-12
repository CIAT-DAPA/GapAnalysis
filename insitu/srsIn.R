srs_insitu <- function(species,Workspace,run_version){
  
  par_dir <- paste0(Workspace,"/","parameters")
  folderin_raw <- paste0(Workspace,"/","parameters","/","occurrences")
  sp_dir <- paste0(Workspace,"/gap_analysis","/",species,"/",run_version)
  model_input_dir <- paste0(Workspace,"/parameters/models/",species)
  occ_dir <- paste0(Workspace,"/","parameters","/","occurrences")
  
  proArea <- raster::raster(paste0(par_dir,"/raster/","wdpa_reclass.tif"))
  cleanPoints <- read.csv(paste0(occ_dir,'/original/',species,'_original.csv'),header=T)[,1:2]#[,c("lon","lat")]
  colnames(cleanPoints) <- c("lon","lat")
  coordinates(cleanPoints) <- ~lon + lat

  totalNum <- nrow(cleanPoints@coords)

  # set coodinate systems equal  
  crs(cleanPoints) = crs(proArea)
  # run a extract to values and select all data. Test for NA, then sum true values for total number of points in protected
  #areas 
  protectPoints <- sum(!is.na(raster::extract(x = proArea,y = cleanPoints)))
  
  #define SRS 
  if(protectPoints >= 0 ){
    srsInsitu <- 100 *(protectPoints/totalNum)
  }else{
    srsInsitu <- 0
  }
  
    #create data.frame with output
  out_df <- data.frame(ID=species, 
                       NTOTAL=totalNum,
                       ProTotal = protectPoints,
                       SRS=srsInsitu)
  write.csv(out_df,paste0(sp_dir,"/gap_analysis/insitu/srs_result.csv"),row.names=F)
  
  #return object
  return(out_df)
}

#Workspace = "E:/CIAT/workspace/Workspace_test/workspace"
#run_version="v1"
#species_list <- c(
#  "Cucurbita_cordata",
#  "Cucurbita_digitata",
#  "Cucurbita_foetidissima",
#  "Cucurbita_palmata"
#)

#lapply(1:length(species_list),function(i){
#  species <- species_list[[i]]
# x <-  srs_insitu(species_list[[i]],Workspace,run_version)
# cat(paste0(species," DONE!"),"\n")
# })
