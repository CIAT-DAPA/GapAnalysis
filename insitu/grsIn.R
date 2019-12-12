insitu_grs = function(species,Workspace,run_version) {
  
suppressMessages(require(rgdal))
suppressMessages(require(raster))
suppressMessages(require(tmap))
suppressMessages(require(fasterize))
suppressMessages(require(sf))
  
data(World)
countries_sh <- World
# sf::st_crs(countries_sh)
countries_sh <- lwgeom::st_transform_proj(countries_sh,"+proj=lonlat",type="proj");x_crs <- sf::st_crs(countries_sh)
suppressWarnings(countries_sh <-as(countries_sh, 'Spatial'))
  
par_dir <- paste0(Workspace,"/","parameters")
folderin_raw <- paste0(Workspace,"/","parameters","/","occurrences")
sp_dir <- paste0(Workspace,"/gap_analysis","/",species,"/",run_version)
model_input_dir <- paste0(Workspace,"/parameters/models/",species)
sp_counts <- read.csv(paste0(sp_dir,"/counts.csv"))
  
proArea <- raster::raster(paste0(par_dir,"/raster/","wdpa_reclass.tif"))
thrshold <- raster::raster(paste0(model_input_dir,"/","spdist_thrsld.tif"))
nativeArea <-  crop(countries_sh,thrshold)
nativeArea <- fasterize::fasterize(sf::st_as_sf(nativeArea), thrshold)
crs(nativeArea)  <- crs(proArea)
        #GRSin = area in protect areas / total area * 100
      # mask protect area to native area 
  proNative <- raster::crop(proArea,nativeArea)
      # add threshold raster to protect areas 
  protectSDM <- thrshold * proNative 
  writeRaster(x = protectSDM,filename = paste0(sp_dir,"/modeling/grs_pa_PAs_narea_areakm2.tif" ), overwrite=TRUE)

      # exclude protected areas outside of model threshold 
      thrshold2 <- thrshold
      thrshold2[which(thrshold2[] == 0)] <- NA
      proNative <- proNative *thrshold
      #calculated the area of cells with in protect areas within the threshold area
      proNative[which(proNative[] == 0)] <- NA
      cell_size <- raster::area(proNative, na.rm=TRUE, weights=FALSE)
      cell_size <- cell_size[!is.na(cell_size)]
      protect_area <- length(cell_size)*median(cell_size)
      # complete for threshold predicted area 
      cell_size <- raster::area(thrshold, na.rm=TRUE, weights=FALSE)
      cell_size <- cell_size[!is.na(cell_size)]
      thrshold_area <- length(cell_size)*median(cell_size)
      
      #calculate GRS
      grs <- min(c(100, protect_area/thrshold_area*100))
      
    #create data.frame with output
    df <- data.frame(ID = species, SPP_AREA_km2 = thrshold_area, SPP_WITHIN_PA_AREA_km2 = protect_area, GRS = grs)
    write.csv(df,paste(sp_dir,"/gap_analysis/insitu/grs_result.csv",sep=""),row.names=F)
    
    return(df)
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
#   x <- insitu_grs(species,Workspace,run_version)
#   cat(paste0(species," DONE!"),"\n")
# })
