#' @title Germplasm representativeness score estimation (In-situ conservation indicators).
#' @name grs_insitu
#' @description Performs an estimation of germplasm representativeness score for in-situ gap analysis (GRSin) using Khoury et al., (2019) methodology
#' This function uses a germplasm buffer raster file (e.g. CA50), a thresholded species distribution model, and a raster file of protected areas
#'  \deqn{GRSin = min(100,(Germplasm buffer area into protected area/species distribution area in protected areas)*100)}
#'
#' @param species A name species compiled using '_'  to call the raster files (SDM and germplasm buffer)
#'  from Workspace/parameters/inputs folder and occurrences files from Workspace/parameter/occurrences folder
#' @param Workspace A forder where the pipeline will be executed
#' @param  run_version The version of the analysis used (e.g 'v1')
#'
#' @return It returns a raster file with the species distribution model restricted to the protected areas raster file provided.
#' Also, this function returns a data frame file saved at gap_analysis folder with four columns:
#'
#' \tabular{lcc}{
#'  ID \tab Species name \cr
#'  SPP_AREA_km2 \tab Area occupied by the species using as input a SDM thresholded file in tiff format \cr
#'  SPP_WITHIN_PA_AREA_km2 \tab Area occupied by the germplasm accessions in a species distribution model \cr
#'  GRS \tab GRSex result \cr
#' }
#'
#' @examples grs_exsitu('Cucurbita_digitata',Workspace,'v1')
#'  \dontrun
#'  Workspace  <-  'E:/CIAT/workspace/Workspace_test/workspace'
#'  run_version  <- 'v1'
#'  species_list <- c('Cucurbita_cordata',
#'   'Cucurbita_digitata',
#'   'Cucurbita_foetidissima',
#'   'Cucurbita_palmata')
#'
#'   run_version <-'v1'
#
#'  lapply(1:length(species_list),function(i){
#'     species <- species_list[[i]]
#'     x <- grs_insitu(species,Workspace,run_version)
#'     print(paste0(species,' DONE!'))
#'  })
#'
#'@references
#'
#'Ramírez-Villegas, J., Khoury, C., Jarvis, A., Debouck, D. G., & Guarino, L. (2010).
#'A Gap Analysis Methodology for Collecting Crop Genepools: A Case Study with Phaseolus Beans.
#'PLOS ONE, 5(10), e13497. Retrieved from https://doi.org/10.1371/journal.pone.0013497
#'
#' Khoury, C. K., Amariles, D., Soto, J. S., Diaz, M. V., Sotelo, S., Sosa, C. C., … Jarvis, A. (2019).
#' Comprehensiveness of conservation of useful wild plants: An operational indicator for biodiversity
#' and sustainable development targets. Ecological Indicators. https://doi.org/10.1016/j.ecolind.2018.11.016
#'
#' @export

insitu_grs = function(species,Workspace,run_version) {

suppressMessages(require(rgdal))
suppressMessages(require(raster))
suppressMessages(require(tmap))
suppressMessages(require(fasterize))
suppressMessages(require(sf))

#importFrom("methods", "as")
#importFrom("stats", "complete.cases", "filter", "median")
#importFrom("utils", "data", "memory.limit", "read.csv", "write.csv")

data(World)
countries_sh <- World
# sf::st_crs(countries_sh)
countries_sh <- lwgeom::st_transform_proj(countries_sh,"+proj=lonlat",type="proj");x_crs <- sf::st_crs(countries_sh)
suppressWarnings(countries_sh <-as(countries_sh, 'Spatial'))

par_dir <- paste0(Workspace,"/","parameters")
folderin_raw <- paste0(Workspace,"/","parameters","/","occurrences")
sp_dir <- paste0(Workspace,"/gap_analysis","/",species,"/",run_version)
model_input_dir <- paste0(Workspace,"/parameters/inputs")
sp_counts <- read.csv(paste0(sp_dir,"/counts.csv"))

proArea <- raster::raster(paste0(par_dir,"/raster/","wdpa_reclass.tif"))
thrshold <- raster::raster(paste0(model_input_dir,"/",species,"_","spdist_thrsld.tif"))
nativeArea <-  raster::crop(countries_sh,thrshold)
nativeArea <- fasterize::fasterize(sf::st_as_sf(nativeArea), thrshold)
raster::crs(nativeArea)  <- raster::crs(proArea)
        #GRSin = area in protect areas / total area * 100
      # mask protect area to native area
  proNative <- raster::crop(proArea,nativeArea)
      # add threshold raster to protect areas
  protectSDM <- thrshold * proNative
  raster::writeRaster(x = protectSDM,filename = paste0(sp_dir,"/modeling/grs_pa_PAs_narea_areakm2.tif" ), overwrite=TRUE)

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
