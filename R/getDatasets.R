
#' @title Download datasets from the harvard dataverse repo
#' @name getDatasets
#' @description
#' Ecoregions and protected area data base are stored on a harvard dataverse repository. This functions check to see if
#' those datasets have been download and will download them if not present.
#'
#'
#'
#' @examples
#  getDatasets()
#'
#' @references
#' Khoury et al. (2019) Ecological Indicators 98:420-429. doi: 10.1016/j.ecolind.2018.11.016
#' Carver et al. (2021) GapAnalysis: an R package to calculate conservation indicators using spatial information
#' @importFrom dataverse get_file
#' @importFrom terra rast writeRaster
#' @export

getDatasets <- function(){
  #LOADING FOLDER PARAMETERS
  out_dir <- system.file(package = "GapAnalysis")#"E:/EG3TC"#system.file("", package = "GapAnalysis")
  example_dir <- paste0(out_dir,"/","data","/","preloaded_data");if(!file.exists(example_dir)){dir.create(example_dir)}
  prot_dir <- paste0(example_dir,"/","protectedArea");if(!file.exists(prot_dir)){dir.create(prot_dir)}
  ecoRegion_dir <- paste0(example_dir,"/","ecoRegion");if(!file.exists(ecoRegion_dir)){dir.create(ecoRegion_dir)}


  # WDPA file  --------------------------------------------------------------
  proAreaPath <- paste0(prot_dir,"/wdpa_rasterize_all.tif")
  if(!file.exists(proAreaPath)){
    # raw protected areas data
    raw_tif_data <- dataverse::get_file(
      file ="wdpa_rasterized_all.tif",
      dataset = "wdpa_rasterized_all.tif",
      server = "https://dataverse.harvard.edu/dataverse/GapAnalysis"
    )
    # Write the raw data to a temporary file on your disk
    temp_file_path <- tempfile(fileext = ".tif")
    writeBin(raw_tif_data, temp_file_path)

    # Step 5: Read the temporary TIFF file into R using terra
    # The 'rast()' function from the terra package reads the raster data.
    raster_data <- terra::rast(temp_file_path)
    terra::writeRaster(x = raster_data, filename = proAreaPath)

  } else {
    message("Protected areas file has been downloaded","\n")
  }


  # ecoregion file ----------------------------------------------------------
  ecoRegionPath <- paste0(prot_dir,"/tnc_terr_ecoregions.gpkg")
  if(!file.exists(ecoRegionPath)){
    # raw data
    raw_eco_data <- dataverse::get_file(
      file ="tnc_terr_ecoregions.gpkg",
      dataset = "doi:10.7910/DVN/WTLNRG",
      server = "https://dataverse.harvard.edu/dataverse/GapAnalysis"
    )
    # Write the raw data to a temporary file on your disk
    temp_file_path <- tempfile(fileext = ".gpkg")
    writeBin(raw_eco_data, temp_file_path)

    # Step 5: Read the temporary TIFF file into R using terra
    # The 'rast()' function from the terra package reads the raster data.
    vect_data <- terra::vect(temp_file_path)
    terra::writeRaster(x = vect_data, filename = ecoRegionPath)

  } else {
    message("Ecoregions files have been downloaded","\n")
  }
  return(message("DATASETS WERE DOWNLOADED!"))
}

