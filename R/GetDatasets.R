#' @title Preparing datasets to run GapAnalysis functions
#' @name GetDatasets
#' @description This function creates the data_preloaded folder and it downloads datasets from dataverse to allow run gapAnalysis package function.
#' @return  This function downloads data required to run a full gap analysis. It creates the folder data_preloaded into the GapAnalysis.
#' Please run this function after install the package.
#' @examples
#' \dontrun{
#' GetDatasets()
#' }
#'
#' @references
#' Khoury, CK, Carver, D, Kates, HR, et al. Distributions, conservation status, and abiotic stress tolerance potential of wild cucurbits (Cucurbita L.).
#' Plants, People, Planet. 2019; 00: 1– 15. https://doi.org/10.1002/ppp3.10085
#'
#' @export
#' @importFrom dataverse get_dataset
#' @import utils

GetDatasets <- function(){

out_dir <- system.file(package = "GapAnalysis")#"E:/EG3TC"#system.file("", package = "GapAnalysis")
example_dir <- paste0(out_dir,"/","preloaded_data");if(!file.exists(example_dir)){dir.create(example_dir)}
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
  cat("SKIPPING RMD FILE DOWNLOAD","\n")
}



#TNC

    data_tnc <- dataverse::get_dataset("doi:10.7910/DVN/WTLNRG") #TNC
    lapply(1:nrow(data_tnc$files),function(i){
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
  cat("SKIPPING WDPA FILE DOWNLOAD","\n")
}
  return("DATASETS WERE DOWNLOADED!")
}
