#' @title Environmental representativeness score estimation (Ex-situ conservation indicators).
#' @name ers_exsitu
#' @description Performs an estimation of enviromental representativeness score for ex-situ gap analysis (ERSex) using Ramirez-Villegas et al., (2010) methodology,
#' this function uses a csv with coordinates, a germplasm buffer raster file and a vectorial file of eccoregions
#'  \deqn{ERSex = min(100,(Number of ecoregions where germplasm accessions are available/
#' Number of ecoregions where species is available)*100)}
#'
#' @param species A name species compiled using '_' to call the raster files (SDM and germplasm buffer)
#' from Workspace/parameters/inputs folder and occurrences files from Workspace/parameter/occurrences folder
#' @param Workspace A forder where the pipeline will be executed
#' @param  run_version The version of the analysis used (e.g 'v1')
#'
#' @return It returns a data frame file saved at gap_analysis folder with four columns:
#'
#'
#'#' \tabular{lcc}{
#'  ID \tab Species name \cr
#'  SPP_N_ECO \tab Number of ecosystems where the species was located \cr
#'  G_N_ECO \tab Number of ecosystems where germplasm accessions were available \cr
#'  ERS \tab ERSex result \cr
#' }
#'
#'
#'
#'
#' @examples ers_exsitu('Cucurbita_digitata',Workspace,'v1')
#'
#' Workspace  <-  'E:/CIAT/workspace/Workspace_test/workspace'
#' run_version  <- 'v1'
#' species_list <- c('Cucurbita_cordata',
#'  'Cucurbita_digitata',
#'  'Cucurbita_foetidissima',
#'  'Cucurbita_palmata')
#'
#'  run_version <-'v1'
#
#' lapply(1:length(species_list),function(i){
#'    species <- species_list[[i]]
#'    x <- ers_exsitu(species,Workspace,run_version)
#'    print(paste0(species,' DONE!'))
#' })
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

#' @export

##########################################   Start Functions    ###############################################
# This function calculates the ERSex It loads occurrences if they exist, then
# loads the presence/absence surface, creates the G buffer (i.e. CA50) and finally
# outputs the ERS and # eco classes in a data.frame (which is written into a file).
# @param (string) species: species ID
# @param (logical) debug: whether to save or not the intermediate raster outputs
# @return (data.frame): This function returns a data frame with ERS, # eco classes
#                       of G buffer (i.e. CA50) and of the presence/absence surface.

ers_exsitu <- function(species,Workspace,run_version) {

#load packages
suppressMessages(require(sp))
suppressMessages(require(raster))
suppressMessages(require(dplyr))
suppressMessages(require(tidyr))

#importFrom("methods", "as")
#importFrom("stats", "complete.cases", "filter", "median")
#importFrom("utils", "data", "memory.limit", "read.csv", "write.csv")

#load counts
par_dir <- paste0(Workspace,"/","parameters")
folderin_raw <- paste0(Workspace,"/","parameters","/","occurrences")
sp_dir <- paste0(Workspace,"/gap_analysis","/",species,"/",run_version)
model_input_dir <- paste0(Workspace,"/parameters/inputs")
occ_dir <- paste0(Workspace,"/","parameters","/","occurrences")
# ecoReg <- raster::raster(paste0(par_dir,"/","raster","/","wwf_eco_terr_geo.tif"))
ecoReg <- raster::shapefile(paste0(par_dir,"/","raster","/ecoreg/","tnc_terr_ecoregions"))


sp_counts <- read.csv(paste0(sp_dir,"/counts.csv"))
cleanPoints <- read.csv(paste0(occ_dir,'/original/',species,'_original.csv'),header=T)[,1:2]#[,c("lon","lat")]
colnames(cleanPoints) <- c("lon","lat")
sp::coordinates(cleanPoints) <- ~lon + lat
sp::proj4string(cleanPoints) <- proj4string(ecoReg)


# number of ecoregions present in model
    ecoVal <- data.frame(over(x = cleanPoints, y = ecoReg))%>%
      dplyr::select(ECO_NUM )%>% #ECO_ID
      distinct() %>%
      drop_na() %>%
      filter(ECO_NUM > 0) # -9998 are lakes #ECO_ID != -9998


    #run only for spp with occ file
    if (!file.exists(paste0(par_dir,"/","inputs","/",species,"_","ca50_G_narea.tif"))) {
      ers <- 0
      ecoValsGLen <- NA
      ecoValsAllPointsLen <- nrow(ecoVal)
    }else{

      #load g buffer
      gBuffer <- raster::raster(paste0(par_dir,"/","inputs","/",species,"_","ca50_G_narea.tif"))
      gBuffer[which(gBuffer[] == 0)] <- NA
      gPoints <- sp::SpatialPoints(raster::rasterToPoints(gBuffer))
      # extract values from ecoregions to points
      raster::crs(gPoints) <- raster::crs(ecoReg)
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
