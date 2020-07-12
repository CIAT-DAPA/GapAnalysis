#' @title Sampling representativeness score ex situ
#' @name SRSex
#' @description The SRSex process provides a general indication of the completeness of ex situ conservation collections,
#'  calculating the ratio of germplasm accessions (G) available in ex situ repositories to reference (H) records for each taxon,
#'  making use of all compiled records, regardless of whether they include coordinates, with an ideal (i.e., comprehensive) conservation
#'  ratio of 1:1. In this and in the subsequent measurements, if no G or H records exist, taxa are automatically considered
#'  to be of high priority for further conservation action and assigned a value of 0. If there are more G than H records,
#' SRSex is set to 100.
#'
#' @param Occurrence_data  A data frame object with the species name, geographical coordinates,
#'  and type of records (G or H) for a given species
#' @param Species_list A species list to calculate the SRSex metrics.
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
#' Cucurbita_splist <- unique(CucurbitaData$taxon)
#' #Running SRSex
#' SRSex_df <- SRSex(Species_list = Cucurbita_splist,
#'                     Occurrence_data = CucurbitaData)
#'
#'@references
#'
#' Khoury et al. (2019) Ecological Indicators 98:420-429. doi: 10.1016/j.ecolind.2018.11.016
#'
#' @export
#' @importFrom fasterize fasterize
#' @importFrom stats median

SRSex <- function(Species_list, Occurrence_data) {

  species <- NULL

  #Checking Occurrence_data format
  par_names <- c("species","latitude","longitude","type")

  if(missing(Occurrence_data)){
    stop("Please add a valid data frame with columns: species, latitude, longitude, type")
  }

  if(isFALSE(identical(names(Occurrence_data),par_names))){
    stop("Please format the column names in your dataframe as species, latitude, longitude, type")
  }




  dt1 <- data.frame(matrix(nrow = length(Species_list), ncol = 2))
  colnames(dt1) <- c("species", "SRSex")

  for(i in seq_len(length(Species_list))){
    sp_counts <- GapAnalysis::OccurrenceCounts(Species_list[i], Occurrence_data)

    if(sp_counts$totalGRecords >= 1 & sp_counts$totalHRecords == 0){
      SRSex <-100
    }

    #### this works for full distributions
    if (sp_counts$totalGRecords == 0 & sp_counts$totalHRecords ==0) {
      SRSex <- 0
    } else {
      SRSex <- min(c(100,(sp_counts$totalGRecords/sp_counts$totalHRecords)*100))
    }
    # add values to empty df
    dt1$species[i] <- as.character(Species_list[i])
    dt1$SRSex[i] <- SRSex

  }
  return(dt1)
}
