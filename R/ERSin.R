#' @title Environmental representativeness score estimation (In-situ conservation)
#' @name ERSin
#' @description This function performs an estimation of germplasm representativeness score
#'  for in-situ gap analysis (GRSin) using Khoury et al., (2019) methodology
#'  This function uses a germplasm buffer raster file (e.g. CA50),
#'  a thresholded species distribution model, and a raster file of protected areas
#'  \deqn{ERSin = min(100,(Number of Ecoregions_shpions of germplasm occurrences in protected areas/
#' Number of Ecoregions_shpions of predicted Habitat within protected areas)*100)}
#'
#' @param Species_list An species list to calculate the ERSin metrics.
#' @param Occurrence_data A data frame object with the species name, geographical coordinates,
#'  and type of records (G or H) for a given species
#' @param Raster_list A list representing the species distribution models for the species list provided
#'  loaded in raster format. This list must match the same order of the species list.
#' @param Pro_areas A raster file representing protected areas information.
#'  If Pro_areas=NULL the funtion will use a protected area raster file provided for your use after run GetDatasets()
#' @param Ecoregions_shp A shapefile representing Ecoregions_shpions information with a field ECO_NUM representing Ecoregions_shpions Ids.
#' If Ecoregions_shp=NULL the function will use a shapefile provided for your use after run GetDatasets()
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
#' Cucurbita_splist <- unique(CucurbitaData$taxon)
#' ##Obtaining Raster_list
#' data(CucurbitaRasters)
#' CucurbitaRasters <- raster::unstack(CucurbitaRasters)
#' ##Obtaining protected areas raster
#' data(ProtectedAreas)
#' ##Obtaining ecoregions shapefile
#' data(ecoregions)
#'
#' ERSin_df <- ERSin(Species_list = Cucurbita_splist,
#'                    Occurrence_data = CucurbitaData,
#'                    Raster_list = CucurbitaRasters,
#'                    Pro_areas= ProtectedAreas,
#'                    Ecoregions_shp=ecoregions)
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


ERSin <- function(Species_list,Occurrence_data,Raster_list,Pro_areas=NULL,Ecoregions_shp=NULL) {

  taxon <- NULL
  type <- NULL
  longitude <- NULL
  ECO_NUM <- NULL

  #importFrom("methods", "as")
  #importFrom("stats", "complete.cases", "filter", "median")
  #importFrom("utils", "data", "memory.limit", "read.csv", "write.csv")

  #Checking Occurrence_data format
  par_names <- c("taxon","latitude","longitude","type")

  if(identical(names(Occurrence_data),par_names)==FALSE){
    stop("Please format the column names in your dataframe as taxon,latitude,longitude,type")
  }
  #Checking if user is using a raster list or a raster stack
  if(class(Raster_list)=="RasterStack"){
    Raster_list <- raster::unstack(Raster_list)
  } else {
    Raster_list <- Raster_list
  }


  ## ERSin analyzes how well protected areas cover the maxent model with regard to ecosystems covered.
  df <- data.frame(matrix(ncol=2, nrow = length(Species_list)))
  colnames(df) <- c("species", "ERSin")
  # load in protect area raster
  if(is.null(Pro_areas) | missing(Pro_areas)){
    if(file.exists(system.file("data/preloaded_data/protectedArea/wdpa_reclass.tif",
                               package = "GapAnalysis"))){
      Pro_areas <- raster::raster(system.file("data/preloaded_data/protectedArea/wdpa_reclass.tif",
                                            package = "GapAnalysis"))
    } else {
      stop("Protected areas file is not available yet. Please run the function GetDatasets()  and try again")
    }
  } else{
    Pro_areas <- Pro_areas
  }
  # Load in ecoregions shp
  if(is.null(Ecoregions_shp) | missing(Ecoregions_shp)){
    if(file.exists(system.file("data/preloaded_data/ecoRegion/tnc_terr_ecoregions.shp",
                               package = "GapAnalysis"))){
      Ecoregions_shp <- raster::shapefile(system.file("data/preloaded_data/ecoRegion/tnc_terr_ecoregions.shp",
                                                      package = "GapAnalysis"),encoding = "UTF-8")
    } else {
      stop("Ecoregions file is not available yet. Please run the function GetDatasets() and try again")
    }
  } else{
    Ecoregions_shp <- Ecoregions_shp
  }

  for(i in seq_len(length(Species_list))){
    # select threshold map for a given species
    for(j in seq_len(length(Raster_list))){
      if(grepl(j, i, ignore.case = TRUE)){
        sdm <- Raster_list[[j]]
      }
    }
    # select occurrence data for the given species
    occData1 <- Occurrence_data[which(Occurrence_data$taxon==Species_list[i] & !is.na(Occurrence_data$latitude)),]

        sp::coordinates(occData1) <- ~longitude+latitude
    raster::crs(occData1) <- raster::crs(Ecoregions_shp)
    # extract the Ecoregions_shpion values to the points

    ecoVal <- sp::over(x = occData1, y = Ecoregions_shp)
    ecoVal <- data.frame(ECO_NUM=(unique(ecoVal$ECO_NUM)))
    ecoVal <- ecoVal[which(!is.na(ecoVal) & ecoVal>0),]
    # number of Ecoregions_shpions in modeling area
    ecoInSDM <- length(ecoVal)

    # mask protected areas to threshold
    Pro_areas1 <- raster::crop(x = Pro_areas, y=sdm)
    sdm[sdm[] == 0] <- NA
    Pro_areas1 <- sdm * Pro_areas1

    #convert protect area to points
    protectPoints <- sp::SpatialPoints(raster::rasterToPoints(Pro_areas1))
    # extract values from Ecoregions_shpions to points
    raster::crs(protectPoints) <- raster::crs(Ecoregions_shp)


    ecoValsPro <- sp::over(x = protectPoints, y = Ecoregions_shp)
    ecoValsPro <- data.frame(ECO_NUM=(unique(ecoValsPro$ECO_NUM)))
    ecoValsPro <- ecoValsPro[which(!is.na(ecoValsPro) & ecoValsPro>0),]
    ecoInProt <- length(ecoValsPro)

    #clause for 9 in protected area
    if(ecoInProt == 0){
      df$species[i] <- as.character(Species_list[i])
      df$ERSin[i] <- 0
    }else{
      # calculate ers
      ERSin <- min(c(100, (ecoInProt/ecoInSDM)*100))
      df$species[i] <- as.character(Species_list[i])
      df$ERSin[i] <- ERSin
    }
  }
  return(df)
}
