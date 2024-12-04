#' @title Sampling representativeness score in situ
#' @name SRSin
#' @description The SRSin process calculates the proportion of all occurrences of a taxon falling within
#' the distribution model that also fall within a protected area
#' @param occurrence_Data A data frame object with the species name, geographical coordinates,
#'  and type of records (G or H) for a given species
#' @param taxon A character vector with the species name used to index all records from the occurrence_Data
#' @param sdm A terra rast object representing the species distribution models
#' @param protected_Areas A terra rast object representing protected areas.
#'  If Pro_areas=NULL the function will use a protected area raster file
#'  provided for your use after run GetDatasets()
#' @return This function returns a data frame with four columns:
#'
#' \tabular{lcc}{
#' ID \tab Species name \cr
#' NTOTAL  \tab Total number of records within the distribution\cr
#' ProTotal \tab Total number of records within the distribution within a protected area\cr
#' srs_insitu \tab SRSin value calculated\cr
#' }
#'
#' @examples
#' ##Obtaining occurrences from example
#' load("data/CucurbitaData.rda")
#' ##Obtaining species names from the data
#' taxon <- CucurbitaData$species[1]
#' ##Obtaining Raster_list
#' load("data/CucurbitaRasts.rda")
#' ##Obtaining protected areas raster
#' load("data/protectAreasRast.rda")
#' #' #Running SRSin
#' SRSin_df <- SRSin(taxon = taxon,
#'                   occurrence_Data = CucurbitaData,
#'                   sdm = terra::unwrap(CucurbitaRasts)[[1]],
#'                   protected_Areas = terra::unwrap(protectAreasRast))

#'@references
#' Carver et al. (2021) Ecography 44(7):1000-1009. https://doi.org/10.1111/ecog.05430
#' Khoury et al. (2019) Diversity and Distributions 26(2):209-225. doi: 10.1111/DDI.13008.
#'
#' @export
#' @importFrom raster raster crop projection


SRSin <- function(taxon, sdm, occurrence_Data,  protected_Areas = NULL){


  # filter the occurrence data to the species of interest
  d1 <- occurrence_Data |>
    dplyr::filter(species == taxon) |>
    terra::vect(geom=c("longitude", "latitude"))


  # assign a protected areas object is one is not provided.
  if(is.null(protected_Areas)){
    if(file.exists(system.file("data/preloaded_data/protectedArea/wdpa_reclass.tif",package = "GapAnalysis"))){
      protected_Areas <- raster::raster(system.file("data/preloaded_data/protectedArea/wdpa_reclass.tif",package = "GapAnalysis")) |> terra::vect()
    }
  }

  ## if there is occurrence data but no model we can report the total number of
  ## occurrence data within protected areas, basically non spatially bound assessment
  if(class(sdm) != "SpatRaster"){
    totalObservations <- nrow(d1)
    t1 <- terra::extract(x = protected_Areas,y = d1)
  }else{
    mask1 <- ifel(test = sdm == 1, yes = 1, no = NA)
    # determine the number of observations within the threshold
    to <- terra::extract(x = mask1, y = d1)
    # assign names for indexing
    names(to)<- c("ID", "layer")
    #
    totalObservations <- sum(to$layer, na.rm = TRUE)

    # crop protected areas raster
    p1 <- terra::crop(x = protected_Areas, y = mask1)
    # multiple to create mask -- protected areas within the threshold of the model
    p1 <- p1 * mask1
    # extract values from crop
    t1 <- terra::extract(x = p1,y = d1)
    names(t1) <- c("ID", "layer")
  }
  # extracted values are 1 or NA so sum all the values to get the total.
  totalInProtectArea <- sum(t1$layer, na.rm = TRUE)

  #define SRS
  if(totalInProtectArea >= 0 ){
    srsInsitu <- 100 *(totalInProtectArea/totalObservations)
  }else{
    srsInsitu <- 0
  }

  #create data.frame with output
  out_df <- data.frame(ID=d1$species[1],
                       NTOTAL=totalObservations,
                       ProTotal = totalInProtectArea,
                       srs_insitu=srsInsitu)
  return(out_df)
}

