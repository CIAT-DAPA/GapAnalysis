ers_insitu <- function(species,Workspace,run_version) {
  
  suppressMessages(require(sp))
  suppressMessages(require(raster))
  suppressMessages(require(dplyr))
  suppressMessages(require(tidyr))
  suppressMessages(require(rgdal))
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
  occ_dir <- paste0(Workspace,"/","parameters","/","occurrences")
  model_input_dir <- paste0(Workspace,"/parameters/models/",species)
  sp_counts <- read.csv(paste0(sp_dir,"/counts.csv"))
  
  
  ecoReg <- raster::shapefile(paste0(par_dir,"/","raster","/ecoreg/","tnc_terr_ecoregions"))
  cleanPoints <- read.csv(paste0(occ_dir,'/original/',species,'_original.csv'),header=T)[,1:2]#[,c("lon","lat")]
  colnames(cleanPoints) <- c("lon","lat")
  coordinates(cleanPoints) <- ~lon + lat
  sp::proj4string(cleanPoints) <- proj4string(ecoReg)
  proArea <- raster::raster(paste0(par_dir,"/raster/","wdpa_reclass.tif"))
  thrshold <- raster::raster(paste0(model_input_dir,"/","spdist_thrsld.tif"))
  nativeArea <-  crop(countries_sh,thrshold)
  nativeArea <- fasterize::fasterize(sf::st_as_sf(nativeArea), thrshold)
  crs(nativeArea)  <- crs(proArea)
  
  
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

