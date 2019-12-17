#' @title Obtaining species names from a CSV file
#' @name get_sp_names
#' @description Creates a species list reading a CSV file to use in the gap_analysis pipeline
#'
#' @param Workspace A forder where the pipeline will be executed
#' @param species_csv A CSV file name located at /Workspace/parameters/input to be splitted
#'
#' @return It returns a species list in an object
#' @examples clean_records(Workspace,species_csv)
#'
#'  Workspace = "E:/CIAT/workspace/Workspace_test/workspace"
#'  species_csv <- 'Cucurbita_CWR_2019_09_30.csv'
#'
#'  x <- get_sp_names(Workspace,species_csv)
#'
#'@references
#'
#' Khoury, C. K., Amariles, D., Soto, J. S., Diaz, M. V., Sotelo, S., Sosa, C. C., … Jarvis, A. (2019).
#' Comprehensiveness of conservation of useful wild plants: An operational indicator for biodiversity
#' and sustainable development targets. Ecological Indicators. https://doi.org/10.1016/j.ecolind.2018.11.016
#'
#' @export

get_sp_names <- function(Workspace,species_csv) {

  #load packages
  suppressMessages(require(ff))
  suppressMessages(require(ffbase))
  suppressMessages(require(dplyr))
  suppressMessages(require(BBmisc))

  # #importFrom("methods", "as")
  # #importFrom("stats", "complete.cases", "filter", "median")
  # #importFrom("utils", "data", "memory.limit", "read.csv", "write.csv")

  # options("ffbatchbytes"= getOption("ffmaxbytes")/1)
  #
  # if (getRversion()>="2.6.0"){ # memory.limit was silently changed from 2.6.0 to return in MB instead of bytes
  #   options(ffbatchbytes=memory.limit()*(1024^2/100))
  # } else {
  #   options(ffbatchbytes=memory.limit()/100)
  # }

  #### Calling shapefile and using WGS84 proj4string
  folderin_raw <- paste0(Workspace,"/","parameters","/","occurrences")

  #### Testing if it is possible read the csv using ff package

  x <- is.error(try(
    ff::read.table.ffdf(x=NULL,file=paste0(folderin_raw,"/",species_csv),
                        header=T,
                        encoding="UTF-8",
                        sep=",",
                        first.rows = 10000,
                        next.rows=10000,
                        #colClasses=NA,
                        VERBOSE = F #,
                        #na.strings=""
    ),silent=T
  )
  )

  #### Reading csv file with coords

  if(x==F){
    cat("Using ff to read","\n")
    spp_total <- ff::read.table.ffdf(x=NULL,file=paste0(folderin_raw,"/",species_csv),
                                     header=T,
                                     encoding="UTF-8",
                                     sep=",",
                                     first.rows = 10000,
                                     next.rows=10000,
                                     #colClasses=NA,
                                     VERBOSE = F
    )
    #na.strings="")
  } else {
    cat("Using read.csv to read","\n")
    spp_total <- read.csv(paste0(folderin_raw,"/",species_csv), header = T, sep=",",na="") ##read file
    spp_total <- ff::as.ffdf(spp_total)
  }
spp_list <- as.character(unique(spp_total$Taxon_final[]))
return(spp_list)
}
