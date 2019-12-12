
grs_exsitu <- function(species,Workspace,run_version) {
  
suppressMessages(require(rgdal))
suppressMessages(require(raster))
  
folderin_raw <- paste0(Workspace,"/","parameters","/","occurrences")
sp_dir <- paste0(Workspace,"/gap_analysis","/",species,"/",run_version)
model_input_dir <- paste0(Workspace,"/parameters/models/",species)
sp_counts <- read.csv(paste0(sp_dir,"/counts.csv"))
  
  if(!file.exists(paste0(model_input_dir,"/","ca50_G_narea.tif"))){
    grs <- 0
    gBufferRas_area <- 0
    pa_spp_area <- NA
  }else{
  
  # load in ga50 and model outputs 
  gBufferRas <- raster::raster(paste0(model_input_dir,"/","ca50_G_narea.tif"))
  cell_size<-raster::area(gBufferRas, na.rm=TRUE, weights=FALSE)
  cell_size<-cell_size[!is.na(cell_size)]
  gBufferRas_area<-length(cell_size)*median(cell_size)

  pa_spp <<- raster::raster(paste0(model_input_dir,"/spdist_thrsld.tif"))
  cell_size<- raster::area(pa_spp, na.rm=TRUE, weights=FALSE)
  cell_size<- cell_size[!is.na(cell_size)]
  pa_spp_area <<-length(cell_size)*median(cell_size)

  #calculate GRS
  grs <- min(c(100, gBufferRas_area/pa_spp_area*100))
    } 
    
#create data.frame with output
out_df <- data.frame(ID=species, SPP_AREA_km2=pa_spp_area, G_AREA_km2=gBufferRas_area, GRS=grs)
write.csv(out_df,paste(sp_dir,"/gap_analysis/exsitu/grs_result.csv",sep=""),row.names=F)
    
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
#run_version <-"v1"


# lapply(1:length(species_list),function(i){
#   species <- species_list[[i]]
#   x <- grs_exsitu(species,Workspace,run_version)
#   cat(paste0(species," DONE!"),"\n")
# })
