create_buffers <- function(species,Workspace,bufferType,buff_dist,run_version){
suppressMessages(require(rgdal))
suppressMessages(require(maptools))
suppressMessages(require(raster))
suppressMessages(require(rgeos))
suppressMessages(require(fasterize))
#devtools::install_github("valentinitnelav/geobuffer")
suppressMessages(require(geobuffer))
  
occ_dir <- paste0(Workspace,"/","parameters","/","occurrences")
gap_dir <- paste(Workspace,"/","gap_analysis",sep="")
sp_dir <- paste(gap_dir,"/",species,"/",run_version,sep="")
par_dir <- paste(Workspace,"/","parameters",sep="")

##ensure msk has a CRS assigned to
msk <- raster::raster(paste0(Workspace,"/","parameters/raster",'/mask.tif'))
msk_sp <- raster::raster(paste0(Workspace,"/","parameters/models","/",species,"/","spdist_thrsld.tif"))
msk <- raster::crop(msk,msk_sp)
proj4string(msk) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")

  if(bufferType=='total'){
    xy <- read.csv(paste0(occ_dir,'/original/',species,'_original.csv'),header=T)[,1:2]#[,c("lon","lat")]
    colnames(xy) <- c("lon","lat")
    filename_to <- paste0(par_dir,"/","models","/",species,"/","ca50_",bufferType,"_narea.tif")    
  } else if(bufferType=='G'){
    xy <- read.csv(paste0(occ_dir,'/original/',species,'_original.csv'),header=T)#[,c("lon","lat")]    if(headreader==F){
    xy <- xy[which(xy$type=="G"),]
    xy <- xy[,1:2]
    filename_to <- paste0(par_dir,"/","models","/",species,"/","ca50_",bufferType,"_narea.tif")  
    }
  
  if(nrow(xy)>0){
    if (!file.exists(filename_to)) {
      cat('Doing ', species, '\n')

      ##making points spatial object with coordinates
      xy_coords <- as.data.frame(cbind(xy$lon, xy$lat))
      xy_coords <- xy_coords[complete.cases(xy_coords),]
      xy_coords <- unique(xy_coords)
      colnames(xy_coords) <- c("x","y")
      coordinates(xy_coords) <- ~x+y
      sp::proj4string(xy_coords) <- CRS("+proj=longlat +datum=WGS84")
    
      ##buffering
      buffer <- geobuffer::geobuffer_pts(xy = xy_coords, 
                              dist_m = buff_dist, 
                              output = 'sf')
    
      ##rasterizing and making it into a mask
      buffer_rs <- fasterize::fasterize(buffer, msk)
      buffer_rs[which(!is.na(buffer_rs[]))] <- 1
      buffer_rs[which(is.na(buffer_rs[]) & msk[] == 1)] <- 0
      buffer_rs[which(is.na(msk[]))] <- NA
    
       ##writing raster
      raster::writeRaster(buffer_rs, filename_to)
      } else{
    ##load raster in case it exists
    buffer_rs <- raster(filename_to)
      }
    } else {
       cat("No coords available","\n")
    buffer_rs <- NULL
  }  
  ##return object
  return(buffer_rs)
}

# species_list <- c(
#   "Cucurbita_cordata",
#   "Cucurbita_digitata",
#   "Cucurbita_foetidissima",
#   "Cucurbita_palmata"
# )
# 
# # species <- species_list[[1]]
# Workspace = "E:/CIAT/workspace/Workspace_test/workspace"
# run_version <- "v1"
# buff_dist=50000 #50km radius
# bufferType="G"
# 
# lapply(1:length(species_list),function(i){
#   species <- species_list[[i]]
#   x <-  create_buffers(species,Workspace,bufferType,buff_dist,run_version)
#   cat(paste0(species," DONE!"),"\n")
# })