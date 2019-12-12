
gap_map_exsitu <- function(species,Workspace,run_version) {
  
suppressMessages(require(rgdal))
suppressMessages(require(raster))
  
sp_dir <- paste0(Workspace,"/gap_analysis","/",species,"/",run_version)
model_input_dir <- paste0(Workspace,"/parameters/models/",species)
par_dir <- paste0(Workspace,"/","parameters")

if(!file.exists(paste0(sp_dir,"/gap_analysis/exsitu/gap_map.tif"))){
  if(file.exists(paste0(par_dir,"/","models","/",species,"/","ca50_G_narea.tif"))){
    gBuffer <- raster::raster(paste0(par_dir,"/","models","/",species,"/","ca50_G_narea.tif"))
  } else {
    gBuffer <- NULL
  }

  pa_spp <- raster::raster(paste0(model_input_dir,"/spdist_thrsld.tif"))
  pa_spp[which(pa_spp[] == 0)] <- NA
  
  if(!is.null(gBuffer)){
    gap_map <- pa_spp + gBuffer
    gap_map[which(gap_map[]==2)] <- 0
  } else {
    gap_map <- pa_spp  
  }
  raster::writeRaster(gap_map,paste0(sp_dir,"/gap_analysis/exsitu/gap_map.tif"))
} else {
  gap_map <- raster::raster(paste0(sp_dir,"/gap_analysis/exsitu/gap_map.tif"))
}
  return(gap_map)
}

#Workspace = "E:/CIAT/workspace/Workspace_test/workspace"
#run_version="v1"
#species_list <- c(
#  "Cucurbita_cordata",
#  "Cucurbita_digitata",
#  "Cucurbita_foetidissima",
#  "Cucurbita_palmata"
#)
#run_version <-"v1"


#lapply(1:length(species_list),function(i){
#  species <- species_list[[i]]
#  x <- gap_map_exsitu(species,Workspace,run_version)
#  cat(paste0(species," DONE!"),"\n")
#})
