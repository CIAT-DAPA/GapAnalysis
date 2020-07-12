#' @title Creating a summary HTML document for each taxon
#' @name SummaryHTML
#' @description Calls the summaryHTML rmd file information and displays
#'  the quantitative and spatial results content.
#' @param Species_list A species list to calculate metrics.
#' @param Occurrence_data A data frame object with the species name, geographical coordinates,
#'  and type of records (G or H) for a given species
#' @param Raster_list A list representing the species distribution models for the species list provided
#'  loaded in raster format. This list must match the same order as the species list.
#' @param Buffer_distance Geographical distance used to create circular buffers around germplasm.
#'  Default: 50000 that is 50 km around germplasm accessions (CA50)
#' @param Pro_areas A raster file representing protected areas information.
#' If Pro_areas=NULL the funtion will use a protected area raster file provided for your use after run GetDatasets()
#' @param Ecoregions_shp A shapefile representing Ecoregions information with a field ECO_ID_U representing Ecoregions Ids.
#'  If Ecoregions=NULL the function will use a shapefile provided for use after running GetDatasets()
#' @param Output_Folder A path to save the HTML file resulting of this function
#' @param writeRasters Boolean field (default=F) to indicate if raster files should be saved
#'
#' @return This function returns a data frame file saved at a specified folder
#' @examples
#' \dontrun{
#' ##Obtaining occurrences from example
#' data(CucurbitaData)
#' ##Obtaining species names from the data
#' Cucurbita_splist <- unique(CucurbitaData$species)
#' ##Obtaining raster_list
#' data(CucurbitaRasters)
#' CucurbitaRasters <- raster::unstack(CucurbitaRasters)
#' ##Obtaining protected areas raster
#' data(ProtectedAreas)
#' ##Obtaining ecoregions shapefile
#' data(ecoregions)
#' ##Running SummaryHTML function
#' summaryHTML_file <- SummaryHTML(Species_list=Cucurbita_splist,
#'                                  Occurrence_data = CucurbitaData,
#'                                  Raster_list=CucurbitaRasters,
#'                                  Pro_areas=ProtectedAreas,
#'                                  Buffer_distance=50000,
#'                                  Output_Folder="./",
#'                                  writeRasters=F)
#' }
#'
#'@references
#'
#' Khoury et al. (2019) Diversity and Distributions 26(2):209-225. doi: 10.1111/DDI.13008
#'
#' @export
#' @importFrom rmarkdown render
#' @importFrom tmap tmap_mode qtm
#' @importFrom raster raster extend writeRaster crop extent
#' @importFrom sp coordinates proj4string CRS


SummaryHTML <- function(Species_list, Occurrence_data, Raster_list,Buffer_distance=50000,Ecoregions_shp=NULL,Pro_areas=NULL,
                         Output_Folder, writeRasters){
  out_dir <- system.file(package = "GapAnalysis")


  if(missing(Occurrence_data)){
    stop("Please add a valid data frame with columns: species, latitude, longitude, type")
  }


  if(!file.exists(paste0(out_dir,"/data/","preloaded_data","/","summaryHTML.Rmd"))){
    stop("Rmd file is not available yet. Please run the function GetDatasets() and try again")
    } else {
      for(i in 1:length(Species_list)){
        Sl <- Species_list[i]
        Od <- Occurrence_data[Occurrence_data$taxon == Species_list[i], ]
        if(class(Raster_list)=="RasterStack"){
          Raster_list <- raster::unstack(Raster_list)
        } else {
          Raster_list <- Raster_list
        }
        Rl <- Raster_list[[i]]
        rmarkdown::render(input = paste0(out_dir,"/data/","preloaded_data","/","summaryHTML.Rmd"),
                          output_dir = Output_Folder,
                          output_file  = paste(as.character(Species_list[i]),"_SummaryReport.html")
        )
      }
  }
}
