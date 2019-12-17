#' @title Creating buffer around germplasm accessions extracted from a CSV file (Pre-analysis function)
#' @name create_buffers
#' @description Creates a raster using circular buffer around germplasm accessions for a given species using a CSV file
#'  and the R packages fasterize and geobuffer
#'
#' @param species A name species compiled using '_'  to call occurrences files from Workspace/parameter/occurrences folder
#' @param Workspace A forder where the pipeline will be executed
#' @param buff_dist Distance around germplasm accessions geographical coordinates in km^2
#' @param  run_version The version of the analysis used (e.g 'v1')
#'
#' @return It returns a raster file saved in /Workspace/parameters/input to be used to calculate ex-situ conservation indicators
#'
#' @examples create_buffers(species,Workspace,buff_dist,run_version)
#'
#'  dir  <-  'E:/CIAT/workspace/Workspace_test'
#'  run_version  <- 'v1'
#'  species_list <- c('Cucurbita_cordata',
#'   'Cucurbita_digitata',
#'   'Cucurbita_foetidissima',
#'   'Cucurbita_palmata')
#'
#'  x <- create_buffers(species,Workspace,buff_dist,run_version)
#'
#'@references
#'
#' Khoury, C. K., Amariles, D., Soto, J. S., Diaz, M. V., Sotelo, S., Sosa, C. C., … Jarvis, A. (2019).
#' Comprehensiveness of conservation of useful wild plants: An operational indicator for biodiversity
#' and sustainable development targets. Ecological Indicators. https://doi.org/10.1016/j.ecolind.2018.11.016
#'
#' @export

create_buffers <- function(species,Workspace,buff_dist,run_version){

suppressMessages(require(rgdal))
suppressMessages(require(maptools))
suppressMessages(require(raster))
suppressMessages(require(rgeos))
suppressMessages(require(fasterize))
#devtools::install_github("valentinitnelav/geobuffer")
suppressMessages(require(geobuffer))

#importFrom("methods", "as")
#importFrom("stats", "complete.cases", "filter", "median")
#importFrom("utils", "data", "memory.limit", "read.csv", "write.csv")

occ_dir <- paste0(Workspace,"/","parameters","/","occurrences")
gap_dir <- paste(Workspace,"/","gap_analysis",sep="")
sp_dir <- paste(gap_dir,"/",species,"/",run_version,sep="")
par_dir <- paste(Workspace,"/","parameters",sep="")

##ensure msk has a CRS assigned to
msk <- raster::raster(paste0(Workspace,"/","parameters/raster",'/mask.tif'))
msk_sp <- raster::raster(paste0(Workspace,"/","parameters/inputs","/",species,"_","spdist_thrsld.tif"))
msk <- raster::crop(msk,msk_sp)
msk <- raster::mask(msk,msk_sp)
sp::proj4string(msk) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
bufferType <- "G"
xy <- read.csv(paste0(occ_dir,'/original/',species,'_original.csv'),header=T)[,1:2]#[,c("lon","lat")]
colnames(xy) <- c("lon","lat")
filename_to <- paste0(par_dir,"/","inputs","/",species,"_","ca50_",bufferType,"_narea.tif")

  if(nrow(xy)>0){
    if (!file.exists(filename_to)) {
      cat('Doing ', species, '\n')

      ##making points spatial object with coordinates
      xy_coords <- as.data.frame(cbind(xy$lon, xy$lat))
      xy_coords <- xy_coords[complete.cases(xy_coords),]
      xy_coords <- unique(xy_coords)
      colnames(xy_coords) <- c("x","y")
      sp::coordinates(xy_coords) <- ~x+y
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
#
# lapply(1:length(species_list),function(i){
#   species <- species_list[[i]]
#   x <-  create_buffers(species,Workspace,buff_dist,run_version)
#   cat(paste0(species," DONE!"),"\n")
# })
