#' @title Germplasm representativeness score estimation (In-situ conservation indicators).
#' @name ers_insitu
#' @description Performs an estimation of germplasm representativeness score for in-situ gap analysis (GRSin) using Khoury et al., (2019) methodology
#' This function uses a germplasm buffer raster file (e.g. CA50), a thresholded species distribution model, and a raster file of protected areas
#'  \deqn{ERSin = min(100,(Number of ecoregions where germplasm accessions in protected areas/
#' Number of ecoregions where species distribution is intersected with protected areas)*100)}
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
#'  SPP_N_ECO \tab Number of ecosystems where species distribution model is within protected areas \cr
#'  SPP_WITHIN_PA_N_ECO \tab Number of ecosystems where germplasm accessions are within protected areas \cr
#'  ERS \tab ERSex result \cr
#' }
#'
#'
#' @examples ers_insitu('Cucurbita_digitata',Workspace,'v1')
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
#'     x <- ers_insitu(species,Workspace,run_version)
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
ers_insitu <- function(species,Workspace,run_version) {

  suppressMessages(require(sp))
  suppressMessages(require(raster))
  suppressMessages(require(dplyr))
  suppressMessages(require(tidyr))
  suppressMessages(require(rgdal))
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
  occ_dir <- paste0(Workspace,"/","parameters","/","occurrences")
  model_input_dir <- paste0(Workspace,"/parameters/inputs")
  sp_counts <- read.csv(paste0(sp_dir,"/counts.csv"))


  ecoReg <- raster::shapefile(paste0(par_dir,"/","raster","/ecoreg/","tnc_terr_ecoregions"))
  cleanPoints <- read.csv(paste0(occ_dir,'/original/',species,'_original.csv'),header=T)[,1:2]#[,c("lon","lat")]
  colnames(cleanPoints) <- c("lon","lat")
  sp::coordinates(cleanPoints) <- ~lon + lat
  sp::proj4string(cleanPoints) <- proj4string(ecoReg)
  proArea <- raster::raster(paste0(par_dir,"/raster/","wdpa_reclass.tif"))
  thrshold <- raster::raster(paste0(model_input_dir,"/",species,"_","spdist_thrsld.tif"))
  nativeArea <-  raster::crop(countries_sh,thrshold)
  nativeArea <- fasterize::fasterize(sf::st_as_sf(nativeArea), thrshold)
  raster::crs(nativeArea)  <- raster::crs(proArea)


    # mask protect area to native area
    proNative <- raster::crop(proArea,nativeArea)
    proNative <- suppressWarnings(proArea*nativeArea)
    # load in protected area maps and convert to points
    protectPoints <- sp::SpatialPoints(raster::rasterToPoints(proNative))
    # extract values from ecoregions to points
    crs(protectPoints) <- crs(ecoReg)
    ecoValsProt <- sp::over(x = protectPoints, y = ecoReg) %>%
      dplyr::select(ECO_NUM )%>% #ECO_ID
      distinct(ECO_NUM ) %>% #ECO_ID
      drop_na()%>%
      filter(ECO_NUM > 0) #ECO_ID
    #number of ecoRegions in protected areas
    ecoInProt <- nrow(ecoValsProt)

    # number of ecoregion in the SDM
    ecoVal <- data.frame(over(x = cleanPoints, y = ecoReg))%>%
      dplyr::select(ECO_NUM )%>% #ECO_ID
      distinct() %>%
      drop_na() %>%
      filter(ECO_NUM > 0) # -9998 are lakes #ECO_ID

    ecoInSDM <- nrow(ecoVal)

    #calculate ERS
    ers <- min(c(100, (ecoInProt/ecoInSDM)*100))
    #create data.frame with output
    df <- data.frame(ID=species, SPP_N_ECO = ecoInSDM, SPP_WITHIN_PA_N_ECO = ecoInProt, ERS = ers)
    write.csv(df,paste0(sp_dir,"/gap_analysis/insitu/ers_result.csv"),row.names=F)
    #return object
    return(df)
}


#Workspace = "E:/CIAT/workspace/Workspace_test/workspace"
#run_version="v1"
#species_list <- c(
# "Cucurbita_cordata",
# "Cucurbita_digitata",
# "Cucurbita_foetidissima",
#  "Cucurbita_palmata"
#)
#run_version <-"v1"

#lapply(1:length(species_list),function(i){
#  species <- species_list[[i]]
#  x <- ers_insitu(species,Workspace,run_version)
#  cat(paste0(species," DONE!"),"\n")
#})

