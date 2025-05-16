
# download data function

# wwf datasets
# https://www.worldwildlife.org/publications/terrestrial-ecoregions-of-the-world

## preprocessing sets
#



GetDatasets <- function(){
  #LOADING FOLDER PARAMETERS
  out_dir <- system.file(package = "GapAnalysis")#"E:/EG3TC"#system.file("", package = "GapAnalysis")
  example_dir <- paste0(out_dir,"/","data","/","preloaded_data");if(!file.exists(example_dir)){dir.create(example_dir)}
  prot_dir <- paste0(example_dir,"/","protectedArea");if(!file.exists(prot_dir)){dir.create(prot_dir)}
  ecoRegion_dir <- paste0(example_dir,"/","ecoRegion");if(!file.exists(ecoRegion_dir)){dir.create(ecoRegion_dir)}
  dataverse_path <- "https://dataverse.harvard.edu/api/access/datafile/" #DOWNLOAD LINK


  #TNC ECOREGIONS
  ecoRegionPath <- paste0(prot_dir,"/tnc_terr_ecoregions.gpkg")
  if(!file.exists(ecoRegionPath)){
    # need to change the url once the upload gets approved
    x <- utils::download.file(url = " https://dataverse.harvard.edu/api/access/datafile/11514933",
                              destfile = ecoRegionPath,
                              mode = "wb")
  } else {
    message("WDPA FILE WAS ALREADY DOWNLOADED","\n")
  }

  # data_tnc <- data.frame(id=c(3678105,3678101,3678100,3678109,3678111,3678110,3678103,3678107,
  #                             3678104,3678108,3678106,3678102),
  #                        label=c("ERchanges120709.doc","layerfiles_readme.txt","terr_biomes.lyr",
  #                                "tnc_terr_ecoregions.dbf","tnc_terr_ecoregions.lyr",
  #                                "tnc_terr_ecoregions.prj","tnc_terr_ecoregions.sbn",
  #                                "tnc_terr_ecoregions.sbx","tnc_terr_ecoregions.shp",
  #                                "tnc_terr_ecoregions.shp.xml","tnc_terr_ecoregions.shx",
  #                                "wdpa_reclass.tif"))
  # lapply(seq_len(nrow(data_tnc)),function(i){
  #   if(!file.exists(paste0(ecoRegion_dir,"/",data_tnc$label[[i]]))){
  #     x <- utils::download.file(paste0(dataverse_path,data_tnc$id[[i]],"?gbrecs=true"),
  #                               paste0(ecoRegion_dir,"/",data_tnc$label[[i]]),
  #                               mode = "wb")
  #   } else {
  #     x <- paste(data_tnc$label[[i]],"already downloaded!")
  #   }
  #   return(x)
  # })

  #PROTECTED AREA
  proAreaPath <- paste0(prot_dir,"/wdpa_rasterize_all.tif")
  if(!file.exists(proAreaPath)){
    #data_prot <- dataverse::get_dataset("doi:10.7910/DVN/XIV9BL") #WDPA
    x <- utils::download.file(url = " https://dataverse.harvard.edu/api/access/datafile/11514930",
                              destfile =paste0(prot_dir,"/wdpa_rasterize_all.tif"),
                              mode = "wb")
  } else {
    message("WDPA FILE WAS ALREADY DOWNLOADED","\n")
  }

  return(message("DATASETS WERE DOWNLOADED!"))
}

