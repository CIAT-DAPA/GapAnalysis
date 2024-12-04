pacman::p_load(raster,terra,dplyr)


## preping the input dataset
load("data/CucurbitaRasters.rda")
CucurbitaRasts <- terra::rast(CucurbitaRasters) |> terra::wrap()
#' # export for terra version
save(CucurbitaRasts, file = "data/CucurbitaRasts.rda")
#'
#' #' ##Obtaining protected areas raster
data(ProtectedAreas)
#' # convert
protectAreasRast <- terra::rast(ProtectedAreas) |> terra::wrap()
save(protectAreasRast, file = "data/protectAreasRast.rda")
