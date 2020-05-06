#' @title Final in-situ conservation score estimation (In-situ conservation)
#' @name FCSin
#' @description This function concatenates the SRSin, GRSin, and ERSin values in an unique dataframe object to calculate a final priority score as
#' the average of the SRSin, GRSin, and ERSin values:
#'
#' \deqn{FCSin = mean(SRSin,GRSin,ERSin)}
#'
#' @param srsDF A dataframe object result of the SRSin function
#' @param grsDF A dataframe object result of the GRSin function
#' @param ersDF A dataframe object result of the ERSin function
#'
#' @return This function returns a data frame with the follows information summarizing the in-situ gap analysis scores:
#'
#' \tabular{lcc}{
#' species \tab Species name \cr
#' SRSin \tab In-situ sample representativeness score \cr
#' GRSin \tab In-situ germplasm representativeness score \cr
#' ERSin \tab In-situ environmental representativeness score \cr
#' FCSin \tab In-situ final conservation score \cr
#' }
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
#' #Calculating SRSin value
#' SRSin_df <- SRSin(species_list = speciesList,
#'                     occurrenceData = CucurbitaData,
#'                     raster_list = CucurbitaRasters,
#'                     proArea=ProtectedAreas)
#'
#' #Calculating GRSin value
#' GRSin_df <- GRSin(species_list = speciesList,
#'                     occurrenceData = CucurbitaData,
#'                     raster_list = CucurbitaRasters,
#'                     proArea=ProtectedAreas)
#'
#' #Calculating ERSin value
#' ERSin_df <- ERSin(species_list = speciesList,
#'                     occurrenceData = CucurbitaData,
#'                     raster_list = CucurbitaRasters,
#'                     proArea=ProtectedAreas,
#'                     ecoReg=ecoregions)
#'
#' #Calculating final conservation for ex-situ gap analysis
#'
#' FCSin_df <- FCSin(srsDF = SRSin_df, grsDF = GRSin_df, ersDF = ERSin_df)
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

FCSin <- function(srsDF,grsDF,ersDF) {
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
