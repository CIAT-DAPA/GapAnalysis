clean_records <- function(Workspace,species_csv,species_list,run_version) {

  #load packages
suppressMessages(require(tmap))
suppressMessages(require(raster))
suppressMessages(require(maptools))
suppressMessages(require(rgdal))
suppressMessages(require(ff))
suppressMessages(require(ffbase))
suppressMessages(require(sf))
suppressMessages(require(dplyr))
suppressMessages(require(BBmisc))
options("ffbatchbytes"= getOption("ffmaxbytes")/1)

if (getRversion()>="2.6.0"){ # memory.limit was silently changed from 2.6.0 to return in MB instead of bytes
  options(ffbatchbytes=memory.limit()*(1024^2/100))
} else {
  options(ffbatchbytes=memory.limit()/100)
}

#### Calling shapefile and using WGS84 proj4string

data(World)
countries_sh <- World
# sf::st_crs(countries_sh)
countries_sh <- lwgeom::st_transform_proj(countries_sh,"+proj=lonlat",type="proj");x_crs <- sf::st_crs(countries_sh)
suppressWarnings(countries_sh <-as(countries_sh, 'Spatial'))
# proj4string(countries_sh) 

folderin_raw <- paste0(Workspace,"/","parameters","/","occurrences")
  
#### Testing if it is possible read the csv using ff package

   x <- is.error(try(
    ff::read.table.ffdf(x=NULL,file=paste0(folderin_raw,"/",species_csv),
                               header=T,
                               encoding="UTF-8",
                               sep=",",
                               first.rows = 10000,
                               next.rows=10000,
                               #colClasses=NA,
                               VERBOSE = F #,
                               #na.strings=""
                               ),silent=T
    )
  )

   #### Reading csv file with coords
   
  if(x==F){
    cat("Using ff to read","\n")
    spp_total <- ff::read.table.ffdf(x=NULL,file=paste0(folderin_raw,"/",species_csv),
                        header=T,
                        encoding="UTF-8",
                        sep=",",
                        first.rows = 10000,
                        next.rows=10000,
                        #colClasses=NA,
                        VERBOSE = F
                        )
                        #na.strings="")
  } else {
    cat("Using read.csv to read","\n")
    spp_total <- read.csv(paste0(folderin_raw,"/",species_csv), header = T, sep=",",na="") ##read file
    spp_total <- as.ffdf(spp_total)   
  }
  
#### Subsetting for eah of the species in an species list.
   
x <- lapply(1:length(species_list),function(i){

  species <- species_list[[i]]
  sub_spp <- spp_total[spp_total$Taxon_final[]==species,]
  sub_spp <- as.ffdf(sub_spp)

  spp_coords <- sub_spp[!is.na(sub_spp$latitude[]),]
  spp_coords <- as.ffdf(spp_coords)
  
  #### Calculating counts csv file for each species.
  
  count_csv <- data.frame(matrix(ncol=7,nrow=1))
  colnames(count_csv)  <-  c("totalRecords","totalUseful","totalGRecords","totalGUseful",	
                             "totalHRecords","totalHUseful","nDB")
 
  count_csv[1,1] <-   if(!is.null(nrow(sub_spp))){
                      nrow(sub_spp)} else {
                      if(class(sub_spp)=="list"){1} else {0}
                        }

  #totalRecords
  count_csv[1,2] <- if(!is.null(nrow(spp_coords))){
                    nrow(spp_coords)} else {
                    if(class(spp_coords)=="list"){1} else {0}  #totalUseful
                    }

  count_csv[1,3] <- if(!is.null(nrow(sub_spp[sub_spp$type[]=="G",]))){
                   nrow(sub_spp[sub_spp$type[]=="G",])} else {  
                    if(class(sub_spp[sub_spp$type[]=="G",])=="list") {1} else {0}
                    }

  count_csv[1,4] <- if(!is.null(nrow(spp_coords[spp_coords$type[]=="G",]))){
                  nrow(spp_coords[spp_coords$type[]=="G",])} else {
                    if(class(spp_coords[spp_coords$type[]=="G",])=="list") {1} else {0}
                    }

  count_csv[1,5] <- if(!is.null(nrow(sub_spp[sub_spp$type[]=="H",]))){
                  nrow(sub_spp[sub_spp$type[]=="H",])} else {
                    if(class(sub_spp[sub_spp$type[]=="H",])=="list") {1} else {0}#totalHRecords
                    }

  count_csv[1,6] <- if(!is.null(nrow(spp_coords[spp_coords$type[]=="H",]))){
                    nrow(spp_coords[spp_coords$type[]=="H",])} else {
                    if(class(spp_coords[spp_coords$type[]=="H",])=="list") { 1} else {0}#totalHUseful
                    }

  count_csv[1,7] <- length(unique(sub_spp$db[])) #nDB


gap_dir <- paste(Workspace,"/","gap_analysis",sep="")
sp_dir <- paste(gap_dir,"/",species,"/",run_version,sep="")
write.csv(count_csv, paste0(sp_dir,'/','counts.csv'),row.names=F,quote=F)

rm(sp_dir,count_csv);gc()

#### Omitting records in the sea and outside the shapefile extent.

  spp <- as.data.frame(spp_coords) 
  spp$lon <- as.numeric(as.character(spp$longitude))
  spp$lat <- as.numeric(as.character(spp$latitude))
  spp <- spp[which(!is.na(spp$lon)),]
  spp <- spp %>% distinct(lon, lat, .keep_all = TRUE) %>%
  sf::st_as_sf(coords = c("lon", "lat"))
  sf::st_crs(spp) <- as.character(crs(countries_sh))
  spp <- as(spp, 'Spatial')
  raster::crs(spp) <- raster::crs(countries_sh)

    if(nrow(spp)>0){
      over_spp <- over(spp, countries_sh) ### over() #overlay
      spp1 <- as.data.frame(spp)
      spp1 <- cbind(spp1, over_spp)
      spp1 <- spp1[which(!is.na(spp1$iso_a3)),]
      spp1 <- spp1[,c("longitude","latitude","type","status","iso_a3")]
      colnames(spp1) <- c("lon","lat","type","status","ISO")
      spp1$native <- NA
      
      
#### Saving cleaned coords file for each species in a list.
      
      paste0(folderin_raw,"/","original")
      write.csv(spp1, paste0(folderin_raw,"/","original","/", species, "_original.csv"), row.names = FALSE,na="")
    } else {
      spp1 <- NULL
    }
cat(paste0(species," CLEAN COORDS DONE!"),"\n")
return(spp1)
  })
}

#system.time(clean_records(Workspace,species_csv,species_list,run_version))
# Workspace = "E:/CIAT/workspace/Workspace_test/workspace"
# 
# species_csv <- "Cucurbita_CWR_2019_09_30.csv"
# run_version="v1"
# species_list <- c(
#   "Cucurbita_cordata",
#   "Cucurbita_digitata",
#   "Cucurbita_foetidissima",
#   "Cucurbita_palmata"
# )
# run_version <-"v1"
