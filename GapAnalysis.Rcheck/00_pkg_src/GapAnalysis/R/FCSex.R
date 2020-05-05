#' @title Final ex situ conservation score estimation (Ex-situ conservation)
#' @name FCSex
#' @description This function concatenates the SRSex, GRSex, and ERSex values in an unique dataframe object to calculate a final priority score as
#' the average of the SRSex, GRSex, and ERSex values:
#'
#' \deqn{FCSex = mean(SRSex,GRSex,ERSex)}
#'
#' @param srsDF A dataframe object result of the SRSex function
#' @param grsDF A dataframe object result of the GRSex function
#' @param ersDF A dataframe object result of the ERSex function
#'
#' @return This function returns a data frame with the follows information summarizing the ex situ gap analysis scores:
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
#' data(cucurbitaData)
#' ##Obtaining species names from the data
#' speciesList <- unique(cucurbitaData$taxon)
#' ##Obtaining raster_list
#' data(cucurbitaRasters)
#' cucurbitaRasters <- raster::unstack(cucurbitaRasters)
#' ##Obtaining ecoregions shapefile
#' data(ecoregions)
#'
#' #Calculating SRSex value
#' SRSex_df <- SRSex(species_list = speciesList,
#'                     occurrenceData = cucurbitaData)
#'
#' #Calculating GRSex value
#' GRSex_df <- GRSex(species_list = speciesList,
#'                     occurrenceData = cucurbitaData,
#'                     raster_list = cucurbitaRasters)
#'
#' #Calculating ERSex value
#' ERSex_df <- ERSex(species_list = speciesList,
#'                     occurrenceData = cucurbitaData,
#'                     raster_list = cucurbitaRasters,
#'                     bufferDistance = 50000,
#'                     ecoReg=ecoregions
#'                     )
#'
#' #Calculating final conservation for ex-situ gap analysis
#'
#' FCSex_df <- FCSex(srsDF = SRSex_df, grsDF = GRSex_df, ersDF = ERSex_df)
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

FCSex <- function(srsDF,grsDF,ersDF){

  #importFrom("methods", "as")
  #importFrom("stats", "complete.cases", "filter", "median")
  #importFrom("utils", "data", "memory.limit", "read.csv", "write.csv")

  # join the dataframes base on species
  df1 <- dplyr::left_join(srsDF, grsDF, by ="species")
  df2 <- dplyr::left_join(df1, ersDF, by = "species") %>%
    dplyr::select("species","SRSex", "GRSex", "ERSex")
  # calculate the mean value for each row to determine fcs per species
  for(i in 1:nrow(df2)){
    df2$FCSex[i] <- base::mean(c(df2$SRSex[i], df2$GRSex[i], df2$ERSex[i]))
  }
  return(df2)
}
