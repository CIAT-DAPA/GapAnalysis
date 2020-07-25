#' @title Preparing datasets to run GapAnalysis functions
#' @name GetDatasets
#' @description This function creates the data_preloaded folder.
#'  It downloads datasets from dataverse to allow the gapAnalysis package to function.
#' @return  This function downloads data required to run a full gap analysis.
#'  It creates the folder data_preloaded into the GapAnalysis.
#' Please run this function after installing the package.
#' @examples
#' \donttest{
#' GetDatasets()
#' }
#' @references
#'
#' Khoury et al. (2019) Data in Brief 22:90-97. doi: 10.1016/j.dib.2018.11.125.
#' Khoury et al. (2019) Diversity and Distributions 26(2):209-225. doi: 10.1111/DDI.1300
#'
#' @export
#' @importFrom dataverse get_dataset
#' @importFrom utils download.file

GetDatasets <- function(){
#LOADING FOLDER PARAMETERS
  out_dir <- system.file(package = "GapAnalysis")#"E:/EG3TC"#system.file("", package = "GapAnalysis")
  example_dir <- paste0(out_dir,"/","data","/","preloaded_data");if(!file.exists(example_dir)){dir.create(example_dir)}
  prot_dir <- paste0(example_dir,"/","protectedArea");if(!file.exists(prot_dir)){dir.create(prot_dir)}
  ecoRegion_dir <- paste0(example_dir,"/","ecoRegion");if(!file.exists(ecoRegion_dir)){dir.create(ecoRegion_dir)}
  dataverse_path <- "https://dataverse.harvard.edu/api/access/datafile/" #DOWNLOAD LINK

#RMD

  if(!file.exists(paste0(example_dir,"/","summaryHTML.Rmd"))){
    data_Rmd<- dataverse::get_dataset("doi:10.7910/DVN/FWYJRK") #Rmd
    x <- utils::download.file(paste0(dataverse_path,data_Rmd$files$id[[1]],"?gbrecs=true"),
                            paste0(example_dir,"/",data_Rmd$files$label[[1]]),
                            mode = "wb")
  } else {
    message("RMD FILE WAS ALREADY DOWNLOADED","\n")
  }

#TNC ECOREGIONS
    data_tnc <- dataverse::get_dataset("doi:10.7910/DVN/WTLNRG") #TNC
    lapply(seq_len(nrow(data_tnc$files)),function(i){
      if(!file.exists(paste0(ecoRegion_dir,"/",data_tnc$files$label[[i]]))){

      x <- utils::download.file(paste0(dataverse_path,data_tnc$files$id[[i]],"?gbrecs=true"),
                paste0(ecoRegion_dir,"/",data_tnc$files$label[[i]]),
                mode = "wb")
      } else {
        x <- paste(data_tnc$files$label[[i]],"already downloaded!")
      }
    return(x)
    })

#PROTECTED AREA
  if(!file.exists(paste0(prot_dir,"/","wdpa_reclass.tif"))){
    data_prot <- dataverse::get_dataset("doi:10.7910/DVN/XIV9BL") #WDPA
    x <- utils::download.file(paste0(dataverse_path,data_prot$files$id[[1]],"?gbrecs=true"),
                     paste0(prot_dir,"/",data_prot$files$label[[1]]),
                     mode = "wb")
    } else {
      message("WDPA FILE WAS ALREADY DOWNLOADED","\n")
    }

  return(message("DATASETS WERE DOWNLOADED!"))
}
