#' @title Final in situ conservation score estimation (In situ conservation)
#' @name fcs_Insitu
#' @description This function concatenates the SRSin, GRSin, and ERSin values in an unique dataframe object to calculate a final priority score as
#' the average of the SRSin, GRSin, and ERSin values:
#'
#' \deqn{FCSex = mean(SRSin,GRSin,ERSin)}
#'
#' @param srsDF A dataframe object result of the srs_insitu function
#' @param grsDF A dataframe object result of the grs_insitu function
#' @param ersDF A dataframe object result of the ers_insitu function
#'
#' @return This function returns a data frame with the follows information summarizing the ex-situ gap analysis scores:
#'
#' \tabular{lcc}{
#' species \tab Species name \cr
#' SRSin \tab In situ sample representativeness score \cr
#' GRSin \tab In situ germplasm representativeness score \cr
#' ERSin \tab In situ environmental representativeness score \cr
#' FCSin \tab In situ final conservation score \cr
#' }
#' @examples
#' ##Obtaining occurrences from example
#' data(cucurbitaData)
#' ##Obtaining species names from the data
#' speciesList <- unique(cucurbitaData$taxon)
#' ##Obtaining raster_list
#' data(sdm_rasters)
#' ##Obtaining protected areas raster
#' data(protectedArea)
#' ##Obtaining ecoregions shapefile
#' data(ecoregions)
#' #Calculating SRSin value
#' SRSin <- srs_Insitu(species_list = speciesList,
#'                     occurrenceData = cucurbitaData,
#'                     raster_list = sdm_rasters,
#'                     proArea=protectedArea)
#'
#' #Calculating GRSin value
#' GRSin <- grs_Insitu(species_list = speciesList,
#'                     occurrenceData = cucurbitaData,
#'                     raster_list = sdm_rasters,
#'                     proArea=protectedArea)
#'
#' #Calculating ERSin value
#' ERSin <- ers_Insitu(species_list = speciesList,
#'                     occurrenceData = cucurbitaData,
#'                     raster_list = sdm_rasters,
#'                     proArea=protectedArea,
#'                     ecoReg=ecoregions)
#'
#' #Calculating final conservation for ex-situ gap analysis
#'
#' fcsIn <- fcs_Insitu(srsDF = SRSin,grsDF = GRSin,ersDF = ERSin)
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
#' @importFrom magrittr %>%
#' @importFrom dplyr left_join select

fcs_Insitu <- function(srsDF,grsDF,ersDF) {
    #importFrom("methods", "as")
    #importFrom("stats", "complete.cases", "filter", "median")
    #importFrom("utils", "data", "memory.limit", "read.csv", "write.csv")

    # join the dataframes base on species
    df1 <- dplyr::left_join(srsDF, grsDF, by ="species")
    df2 <- dplyr::left_join(df1, ersDF, by = "species") %>%
      dplyr::select("species","SRSin", "GRSin", "ERSin")
    # calculate the mean value for each row to determine fcs per species
    for(i in 1:nrow(df2)){
      df2$FCSin[i] <- base::mean(c(df2$SRSin[i], df2$GRSin[i], df2$ERSin[i]))
    }
    return(df2)
  }
