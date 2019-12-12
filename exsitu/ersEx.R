ers_exsitu <- function(species,Workspace,run_version) {

#load packages
suppressMessages(require(sp))
suppressMessages(require(raster))
suppressMessages(require(dplyr))
suppressMessages(require(tidyr))

#load counts
par_dir <- paste0(Workspace,"/","parameters")
folderin_raw <- paste0(Workspace,"/","parameters","/","occurrences")
sp_dir <- paste0(Workspace,"/gap_analysis","/",species,"/",run_version)
model_input_dir <- paste0(Workspace,"/parameters/models/",species)
occ_dir <- paste0(Workspace,"/","parameters","/","occurrences")
# ecoReg <- raster::raster(paste0(par_dir,"/","raster","/","wwf_eco_terr_geo.tif"))
ecoReg <- raster::shapefile(paste0(par_dir,"/","raster","/ecoreg/","tnc_terr_ecoregions"))


sp_counts <- read.csv(paste0(sp_dir,"/counts.csv"))
cleanPoints <- read.csv(paste0(occ_dir,'/original/',species,'_original.csv'),header=T)[,1:2]#[,c("lon","lat")]
colnames(cleanPoints) <- c("lon","lat")
coordinates(cleanPoints) <- ~lon + lat
sp::proj4string(cleanPoints) <- proj4string(ecoReg)


# number of ecoregions present in model 
    ecoVal <- data.frame(over(x = cleanPoints, y = ecoReg))%>%
      dplyr::select(ECO_NUM )%>% #ECO_ID
      distinct() %>%
      drop_na() %>%
      filter(ECO_NUM != c(98,99)) # -9998 are lakes #ECO_ID != -9998
    
    
    #run only for spp with occ file
    if (!file.exists(paste0(sp_dir,"/modeling/alternatives/ga50.tif"))) {
      ers <- 0
      ecoValsGLen <- NA
      ecoValsAllPointsLen <- nrow(ecoVal)
    }else{
      
      #load g buffer 
      gBuffer <- raster::raster(paste0(par_dir,"/","models","/",species,"/","ca50_G_narea.tif"))
      gBuffer[which(gBuffer[] == 0)] <- NA
      gPoints <- sp::SpatialPoints(raster::rasterToPoints(gBuffer))
      # extract values from ecoregions to points 
      crs(gPoints) <- crs(ecoReg)
      ecoValsG <- sp::over(x = gPoints, y = ecoReg) %>%
        distinct(ECO_NUM) %>% #ECO_ID
        filter(ECO_NUM > 0) #ECO_ID
      
      ecoValsGLen <- length(ecoValsG[!is.na(ecoValsG$ECO_NUM),]) #ECO_ID
      
            # number of ecoRegions present in all points 
      ecoValsAllPointsLen <<- nrow(ecoVal)
      
      #calculate ERS
      ers <- min(c(100, (ecoValsGLen/ecoValsAllPointsLen)*100))
      
    }
    #create data.frame with output
    out_df <- data.frame(ID=species, SPP_N_ECO=ecoValsAllPointsLen, G_N_ECO=ecoValsGLen, ERS=ers)
    write.csv(out_df,paste(sp_dir,"/gap_analysis/exsitu/ers_result.csv",sep=""),row.names=F)
    
    #return object
    return(out_df) 
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
# lapply(1:length(species_list),function(i){
#   species <- species_list[[i]]
#   x <- ers_exsitu(species,Workspace,run_version)
#   cat(paste0(species," DONE!"),"\n")
# })
