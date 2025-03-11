#'
#' ##Obtaining occurrences from example
#' load("data/CucurbitaData.rda")
#' ##Obtaining species names from the data
#' taxon <- CucurbitaData$species[1]
#' ##Obtaining Raster_list
#' load("data/CucurbitaRasts.rda")
#' ##Obtaining protected areas raster
#' load("data/protectAreasRast.rda")
#' #' #Running SRSin

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
  out_df <- dplyr::tibble(Taxon = taxon,
                       "Total records" =totalObservations,
                       "Records in Protected areas" = totalInProtectArea,
                       "SRS insitu" = srsInsitu)
  return(out_df)
}

