#' @title Final ex situ conservation score estimation (Ex-situ conservation)
#' @name FCSex
#' @description This function concatenates the SRSex, GRSex, and ERSex values
#'  in an unique dataframe object to calculate a final priority score as
#' the average of the SRSex, GRSex, and ERSex values:
#'
#' \deqn{FCSex = mean(SRSex,GRSex,ERSex)}
#'
#' @param params A list withe follow parameters:
#'  species_list A species list to calculate metrics.
#'  occurrenceData A data frame object with the species name, geographical coordinates,
#'   and type of records (G or H) for a given species
#'  raster_list A list representing the species distribution models for the species list provided
#'   loaded in raster format. This list must match the same order of the species list.
#'  bufferDistance Geographical distance used to create circular buffers around germplasm.
#'   Default: 50000 that is 50 km around germplasm accessions (CA50)
#'  ecoReg A shapefile representing ecoregions information with a field ECO_NUM representing ecoregions Ids.
#'   If ecoReg=NULL the funtion will use a shapefile provided for your use after run GetDatasets()
#'
#' @return This function returns a data frame with the follows information
#' summarizing the ex-situ gap analysis scores:
#'
#' \tabular{lcc}{
#' species \tab Species name \cr
#' SRSex \tab Ex-situ sample representativeness score \cr
#' GRSex \tab Ex-situ germplasm representativeness score \cr
#' ERSex \tab Ex-situ environmental representativeness score \cr
#' FCSex \tab Ex-situ final conservation score \cr
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
#' ##Obtaining ecoregions shapefile
#' data(ecoregions)
#'
#' #Calculating SRSex value
#'FCSex_df <- GapAnalysis::FCSex(species_list=speciesList,
#'                                        occurrenceData=CucurbitaData,
#'                                        raster_list=CucurbitaRasters,
#'                                        bufferDistance=50000,
#'                                        ecoReg=ecoregions)#'
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
#' @importFrom dplyr left_join

params=list(species_list=speciesList,
      occurrenceData=CucurbitaData,
      raster_list=CucurbitaRasters,
      bufferDistance=50000,
      ecoReg=ecoregions)

FCSex <- function(SRS_ex_df,GRS_ex_df,ERS_ex_df, option,params){

  if(option!="Compile" & option!="Manual"){
    stop("No valid option was chosen, please choose Manual or Compile")
  } else if(option=="Compile" & !is.null(params)){
    par_names <- c("species_list","occurrenceData","raster_list","bufferDistance","ecoReg")
    if(identical(names(params),par_names)){
      FCS_ex_df <- GapAnalysis::ExsituCompile(species_list=params$species_list,
                                          occurrenceData=params$occurrenceData,
                                          raster_list=params$raster_list,
                                          bufferDistance=params$bufferDistance,
                                          ecoReg=params$ecoReg)
    } else {
      stop("Check your inputs in the list")
    }
  } else if(option!="Manual" & !is.null(params)){
    stop("Manual mode was chosen, please delete params list")
    } else if(is.null(params) & is.null(SRS_ex_df) & is.null(GRS_ex_df) & is.null(ERS_ex_df)){
    stop("No parameters wered added to calculate FCSex")
  } else if(option=="manual"& is.null(params)){
    # join the dataframes base on species
    FCS_ex_df <- dplyr::left_join(SRS_ex_df, GRS_ex_df, by ="species")
    FCS_ex_df <- dplyr::left_join(FCS_ex_df, ERS_ex_df, by = "species") #%>%
    #    dplyr::select("species","SRSex", "GRSex", "ERSex")
    # calculate the mean value for each row to determine fcs per species
    for(i in seq_len(nrow(FCS_ex_df))){
      FCS_ex_df$FCSex[i] <- base::mean(c(FCS_ex_df$SRSex[i], FCS_ex_df$GRSex[i], FCS_ex_df$ERSex[i]))
    }
  }
  return(FCS_ex_df)
}
