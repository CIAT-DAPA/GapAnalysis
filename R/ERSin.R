#' @title Environmental representativeness score estimation (In-situ conservation)
#' @name ERSin
#' @description This function performs an estimation of germplasm representativeness score
#'  for in-situ gap analysis (GRSin) using Khoury et al., (2019) methodology
#'  This function uses a germplasm buffer raster file (e.g. CA50),
#'  a thresholded species distribution model, and a raster file of protected areas
#'  \deqn{ERSin = min(100,(Number of ecoregions of germplasm occurrences in protected areas/
#' Number of ecoregions of predicted Habitat within protected areas)*100)}
#'
#' @param species_list An species list to calculate the ERSin metrics.
#' @param occurrenceData A data frame object with the species name, geographical coordinates,
#'  and type of records (G or H) for a given species
#' @param raster_list A list representing the species distribution models for the species list provided
#'  loaded in raster format. This list must match the same order of the species list.
#' @param proArea A raster file representing protected areas information.
#'  If proArea=NULL the funtion will use a protected area raster file provided for your use after run GetDatasets()
#' @param ecoReg A shapefile representing ecoregions information with a field ECO_NUM representing ecoregions Ids.
#' If ecoReg=NULL the function will use a shapefile provided for your use after run GetDatasets()
#'
#' @return This function returns a data frame with two columns:
#'
#' \tabular{lcc}{
#' species \tab Species name \cr
#' ERSin \tab ERSin value calculated\cr
#' }
#'
#' @examples
#' ##Obtaining occurrences from example
#' data(CucurbitaData)
#' ##Obtaining species names from the data
#' speciesList <- unique(CucurbitaData$taxon)
#' ##Obtaining raster_list
#' data(CucurbitaRasters)
#' CucurbitaRasters <- raster::unstack(CucurbitaRasters)
#' ##Obtaining protected areas raster
#' data(ProtectedAreas)
#' ##Obtaining ecoregions shapefile
#' data(ecoregions)
#'
#' ERSin_df <- ERSin(species_list = speciesList,
#'                    occurrenceData = CucurbitaData,
#'                    raster_list = CucurbitaRasters,
#'                    proArea= ProtectedAreas,
#'                    ecoReg=ecoregions)
#'
#'@references
#'
#' Ramirez-Villegas, J., Khoury, C., Jarvis, A., Debouck, D. G., & Guarino, L. (2010).
#' A Gap Analysis Methodology for Collecting Crop Genepools: A Case Study with Phaseolus Beans.
#' PLOS ONE, 5(10), e13497. Retrieved from https://doi.org/10.1371/journal.pone.0013497
#'
#' Khoury, C. K., Amariles, D., Soto, J. S., Diaz, M. V., Sotelo, S., Sosa, C. C., … Jarvis, A. (2019).
#' Comprehensiveness of conservation of useful wild plants: An operational indicator for biodiversity
#' and sustainable development targets. Ecological Indicators. https://doi.org/10.1016/j.ecolind.2018.11.016
#'
#' @export
#' @importFrom stats median
#' @importFrom raster raster crop area shapefile


ERSin <- function(species_list,occurrenceData,raster_list,proArea,ecoReg) {

  taxon <- NULL
  type <- NULL
  longitude <- NULL
  ECO_NUM <- NULL

  #importFrom("methods", "as")
  #importFrom("stats", "complete.cases", "filter", "median")
  #importFrom("utils", "data", "memory.limit", "read.csv", "write.csv")

  ## ERSin analyzes how well protected areas cover the maxent model with regard to ecosystems covered.
  df <- data.frame(matrix(ncol=2, nrow = length(species_list)))
  colnames(df) <- c("species", "ERSin")
  # load in protect area raster
  if(is.null(proArea)){
    if(file.exists(system.file("data/preloaded_data/protectedArea/wdpa_reclass.tif",
                               package = "GapAnalysis"))){
      proArea <- raster::raster(system.file("data/preloaded_data/protectedArea/wdpa_reclass.tif",
                                            package = "GapAnalysis"))
    } else {
      stop("Protected areas file is not available yet. Please run the function preparingDatasets()  and try again")
    }
  } else{
    proArea <- proArea
  }
  # Load in ecoregions shp
  if(is.null(ecoReg)){
    if(file.exists(system.file("data/preloaded_data/ecoRegion/tnc_terr_ecoregions.shp",
                               package = "GapAnalysis"))){
      ecoReg <- raster::shapefile(system.file("data/preloaded_data/ecoRegion/tnc_terr_ecoregions.shp",
                                              package = "GapAnalysis"),encoding = "UTF-8")
    } else {
      stop("Ecoregions file is not available yet. Please run the function preparingDatasets() and try again")
      }
  } else{
    ecoReg <- ecoReg
  }

  for(i in seq_len(length(species_list))){
    # select threshold map for a given species
    for(j in seq_len(length(raster_list))){
      if(grepl(j, i, ignore.case = TRUE)){
        sdm <- raster_list[[j]]
      }
    }
    # select occurrence data for the given species
    occData1 <- occurrenceData[which(occurrenceData$taxon==species_list[i] & !is.na(occurrenceData$latitude)),]

        sp::coordinates(occData1) <- ~longitude+latitude
    raster::crs(occData1) <- raster::crs(ecoReg)
    # extract the ecoregion values to the points

    ecoVal <- sp::over(x = occData1, y = ecoReg)
    ecoVal <- data.frame(ECO_NUM=(unique(ecoVal$ECO_NUM)))
    ecoVal <- ecoVal[which(!is.na(ecoVal) & ecoVal>0),]
    # number of ecoregions in modeling area
    ecoInSDM <- length(ecoVal)

    # mask protected areas to threshold
    proArea1 <- raster::crop(x = proArea, y=sdm)
    sdm[sdm[] == 0] <- NA
    proArea1 <- sdm * proArea1

    #convert protect area to points
    protectPoints <- sp::SpatialPoints(raster::rasterToPoints(proArea1))
    # extract values from ecoregions to points
    raster::crs(protectPoints) <- raster::crs(ecoReg)


    ecoValsPro <- sp::over(x = protectPoints, y = ecoReg)
    ecoValsPro <- data.frame(ECO_NUM=(unique(ecoValsPro$ECO_NUM)))
    ecoValsPro <- ecoValsPro[which(!is.na(ecoValsPro) & ecoValsPro>0),]
    ecoInProt <- length(ecoValsPro)

    #clause for 9 in protected area
    if(ecoInProt == 0){
      df$species[i] <- as.character(species_list[i])
      df$ERSin[i] <- 0
    }else{
      # calculate ers
      ers <- min(c(100, (ecoInProt/ecoInSDM)*100))
      df$species[i] <- as.character(species_list[i])
      df$ERSin[i] <- ers
    }
  }
  return(df)
}
