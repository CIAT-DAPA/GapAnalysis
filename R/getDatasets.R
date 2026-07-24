
#' @title Download datasets from the harvard dataverse repo
#' @name getDatasets
#' @description
#' Ecoregions and protected area data base are stored on a harvard dataverse repository. This functions check to see if
#' those datasets have been download and will download them if not present.
#'
#' @return A message confirming the datasets were downloaded, along with saving the files to the package's data directory.
#'
#' @references
#' Khoury et al. (2019) Ecological Indicators 98:420-429. \doi{10.1016/j.ecolind.2018.11.016}
#' Carver et al. (2021) GapAnalysis: an R package to calculate conservation indicators using spatial information
#' @importFrom dataverse get_file
#' @importFrom terra rast writeRaster
#' @export

getDatasets <- function(out_dir = tools::R_user_dir("GapAnalysis", which = "data")){
  if(!file.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

  prot_dir <- file.path(out_dir, "protectedArea")
  if(!file.exists(prot_dir)) dir.create(prot_dir, recursive = TRUE)

  ecoRegion_dir <- file.path(out_dir, "ecoRegion")
  if(!file.exists(ecoRegion_dir)) dir.create(ecoRegion_dir, recursive = TRUE)

  downloaded_any <- FALSE

  proAreaPath <- file.path(prot_dir, "wdpa_rasterized_all.tif")
  if(!file.exists(proAreaPath)){
    raw_tif_data <- dataverse::get_file(
      file = "wdpa_rasterized_all.tif",
      dataset = "doi:10.7910/DVN/KQVOSW",
      server = "https://dataverse.harvard.edu"
    )
    temp_file_path <- tempfile(fileext = ".tif")
    writeBin(raw_tif_data, temp_file_path)
    raster_data <- terra::rast(temp_file_path)
    terra::writeRaster(x = raster_data, filename = proAreaPath)
    downloaded_any <- TRUE
    message("Protected areas file downloaded from Dataverse.\n")
  } else {
    message("Protected areas file already exists.\n")
  }

  ecoRegionPath <- file.path(ecoRegion_dir, "tnc_terr_ecoregions.gpkg")
  if(!file.exists(ecoRegionPath)){
    raw_eco_data <- dataverse::get_file(
      file = "tnc_terr_ecoregions.gpkg",
      dataset = "doi:10.7910/DVN/WTLNRG",
      server = "https://dataverse.harvard.edu"
    )
    temp_file_path <- tempfile(fileext = ".gpkg")
    writeBin(raw_eco_data, temp_file_path)
    vect_data <- terra::vect(temp_file_path)
    terra::writeVector(x = vect_data, filename = ecoRegionPath)
    downloaded_any <- TRUE
    message("Ecoregions file downloaded from Dataverse.\n")
  } else {
    message("Ecoregions file already exists.\n")
  }

  if (downloaded_any) {
    message("Datasets check complete: missing files were downloaded.")
  } else {
    message("Datasets check complete: all files already present; nothing downloaded.")
  }

  invisible(list(
    out_dir = normalizePath(out_dir, mustWork = FALSE),
    protectedArea = normalizePath(proAreaPath, mustWork = FALSE),
    ecoRegion = normalizePath(ecoRegionPath, mustWork = FALSE)
  ))
}
