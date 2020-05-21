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
#' @param Ecoregions_shp A shapefile representing Ecoregions_shpions information with a field ECO_ID_U representing Ecoregions_shpions Ids.
#' If Ecoregions_shp=NULL the function will use a shapefile provided for your use after run GetDatasets()
#' @param Gap_Map Default=FALSE, This option will calculate gap maps for each species analyzed and will retun a list
#' with two slots ERSin and gap_maps
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

ERSin <- function(Species_list,Occurrence_data,Raster_list,Pro_areas=NULL,Ecoregions_shp=NULL,Gap_Map=NULL) {

  taxon <- NULL
  type <- NULL
  longitude <- NULL
  ECO_ID_U <- NULL
  SdmMask <- NULL
  #importFrom("methods", "as")
  #importFrom("stats", "complete.cases", "filter", "median")
  #importFrom("utils", "data", "memory.limit", "read.csv", "write.csv")

  #Checking Occurrence_data format
  par_names <- c("taxon","latitude","longitude","type")

  if(identical(names(Occurrence_data),par_names)==FALSE){
    stop("Please format the column names in your dataframe as taxon,latitude,longitude,type")
  }

  #Checking if GapMapEx option is a boolean
  if(is.null(Gap_Map) | missing(Gap_Map)){ Gap_Map <- FALSE
  } else if(Gap_Map==TRUE | Gap_Map==FALSE){
    Gap_Map <- Gap_Map
  } else {
    stop("Choose a valid option for GapMap (TRUE or FALSE)")
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


  if(Gap_Map==T){
    GapMapIn_list <- list()
  }

  for(i in seq_len(length(Species_list))){
    # select threshold map for a given species
    for(j in seq_len(length(Raster_list))){
      if(grepl(j, i, ignore.case = TRUE)){
        sdm <- Raster_list[[j]]
      }
    }
    # mask protected areas to threshold
    Pro_areas1 <- raster::crop(x = Pro_areas, y=sdm)
    sdm[sdm[] == 0] <- NA
    Pro_areas1 <- sdm * Pro_areas1

    #convert protect area to points
    protectPoints <- sp::SpatialPoints(raster::rasterToPoints(Pro_areas1))
    # extract values from Ecoregions_shpions to points
    raster::crs(protectPoints) <- raster::crs(Ecoregions_shp)
    ecoValsPro <- sp::over(x = protectPoints, y = Ecoregions_shp)
    ecoValsPro <- data.frame(ECO_ID_U=(unique(ecoValsPro$ECO_ID_U)))
    ecoValsPro <- ecoValsPro[which(!is.na(ecoValsPro) & ecoValsPro>0),]
    ecoInProt <- length(ecoValsPro)

    # extract ecoregions values present in predicted presence area
    predictedPresence <- sp::SpatialPoints(raster::rasterToPoints(sdm))
    raster::crs(predictedPresence) <- raster::crs(Ecoregions_shp)
    ecoVal <- sp::over(x = predictedPresence, y = Ecoregions_shp)
    ecoVal <- data.frame(ECO_ID_U=(unique(ecoVal$ECO_ID_U)))
    ecoVal <- ecoVal[which(!is.na(ecoVal) & ecoVal>0),]
    # number of Ecoregions_shpions in modeling area
    ecoInSDM <- length(ecoVal)

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
    if(Gap_Map==T){
      cat("Calculating gap maps for ERSin gap analysis","\n")

      # ERSex Gap Map
      # select all ecoregions present in ecoVal(all points) but absent in ecoValG(g buffers)
      ecoGap <- ecoVal[!ecoVal %in% ecoValsPro]
      if(length(ecoGap) == 0){
        GapMapIn_list[[i]] <- "All ecoregions within the model contain protected areas. There are no gaps"

        }else{

        # pull selected ecoregions and mask to presence area of the model
        eco2 <- Ecoregions_shp[Ecoregions_shp$ECO_ID_U %in% ecoGap,]
        #convert to sf object for conversion using fasterize
        eco2a <- sf::st_as_sf(eco2, SdmMask)
        # generate a ecoregion raster keeping the unique id.
        eco3 <- fasterize::fasterize(eco2a, SdmMask, field = "ECO_ID_U")
        # mask so only locations within the predicted presence area is included.
        gap_map <- eco3 * SdmMask
        GapMapIn_list[[i]] <- gap_map
        names(GapMapIn_list[[i]] ) <- Species_list[[i]]
      }
    }
  }

  if(Gap_Map==T){
    df <- list(ERSin=df, gap_maps = GapMapIn_list )
  }else{
    df <- list(ERSin=df)
  }
  return(df)
}
