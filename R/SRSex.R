#' @title Sample representativeness score estimation (Ex-situ conservation)
#' @name SRSex
#' @description This function performs an estimation of sample representativeness score for ex-situ gap analysis (SRSex)
#' using Ramirez-Villegas et al., (2010) methodology using information from herbarium and germplasm occurrences. SRS ex-situ score is calculated as:
#' \deqn{SRSex = Number of germplasm occurrences / Number of herbarium occurrences}
#'
#' @param occurrenceData  A data frame object with the species name, geographical coordinates, and type of records (G or H) for a given species
#' @param species_list An species list to calculate the SRSex metrics.
#'
#' @return This function returns a data frame with two columns:
#' \tabular{lcc}{
#' species \tab Species name \cr
#' SRSex \tab SRSex value calculated\cr
#' }
#'
#' @examples
#' ##Obtaining occurrences from example
#' data(CucurbitaData)
#' ##Obtaining species names from the data
#' speciesList <- unique(CucurbitaData$taxon)
#' SRSex_df <- SRSex(species_list = speciesList,
#'                     occurrenceData = CucurbitaData)
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
#' @import fasterize sp
#' @importFrom tidyr drop_na
#' @importFrom magrittr "%>%"
#' @importFrom dplyr filter select
#' @importFrom fasterize fasterize
#' @importFrom stats median

SRSex <- function(species_list, occurrenceData) {

  species <- NULL

  dt1 <- data.frame(matrix(nrow = length(species_list), ncol = 2))
  colnames(dt1) <- c("species", "SRSex")

  for(i in 1:length(species_list)){
    sp_counts <- gapAnalysisR::OccurrenceCounts(species_list[i], occurrenceData)

    if(sp_counts$totalGRecords >= 1 & sp_counts$totalHRecords == 0){
      srs <-100
    }

    #### this works for full distributions
    if (sp_counts$totalGRecords == 0 & sp_counts$totalHRecords ==0) {
      srs <- 0
    } else {
      srs <- min(c(100,(sp_counts$totalGRecords/sp_counts$totalHRecords)*100))
    }
    # add values to empty df
    dt1$species[i] <- as.character(species_list[i])
    dt1$SRSex[i] <- srs

  }
  return(dt1)
}
