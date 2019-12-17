#' @title Combining Ex-situ and In-situ gap analysis results in one comprehensive score
#' @name fcs_combine
#' @description Concatenates the SRSin, GRSin, ERSin, SRSex, GRSex, ERSex and Final conservation scores
#'  in one unique CSV file with a combined conservation score using the minimun, average and maximum values between FCSin and FCSex scores
#'  and provides final conservation categories based on the scores above mentioned.
#'
#' @param species A name species compiled using '_'  to call occurrences files from Workspace/parameter/occurrences folder
#' @param Workspace A forder where the pipeline will be executed
#' @param  run_version The version of the analysis used (e.g 'v1')
#'
#' @return It returns a data frame file saved at gap_analysis folder with nive fields:
#'
#' \tabular{lcc}{
#'  ID \tab Species name \cr
#'  FCSex \tab Ex-situ final conservation score \cr
#'  FCSin \tab In-situ final conservation score \cr
#'  FCSc_min \tab Final conservation score mininum value between FCSin and FCSex \cr
#'  FCSc_max \tab Final conservation score maximum value between FCSin and FCSex \cr
#'  FCSc_mean \tab Final conservation score average value between FCSin and FCSex \cr
#'  FCSc_min_class \tab Final conservation category using  FCSc_min value \cr
#'  FCSc_max_class \tab Final conservation category using  FCSc_max value \cr
#'  FCSc_mean_class \tab Final conservation category using  FCSc_mean value \cr
#' }
#'
#' @examples fcs_combine('Cucurbita_digitata',Workspace,'v1')
#'
#' Workspace  <-  'E:/CIAT/workspace/Workspace_test/workspace'
#' run_version  <- 'v1'
#' species_list <- c('Cucurbita_cordata',
#'  'Cucurbita_digitata',
#'  'Cucurbita_foetidissima',
#'  'Cucurbita_palmata')
#'
#'  run_version <-'v1'
#
#' lapply(1:length(species_list),function(i){
#'    species <- species_list[[i]]
#'    x <- fcs_combine(species,Workspace,run_version)
#'    print(paste0(species,' DONE!'))
#' })
#'
#'@references
#'
#' Khoury, C. K., Amariles, D., Soto, J. S., Diaz, M. V., Sotelo, S., Sosa, C. C., … Jarvis, A. (2019).
#' Comprehensiveness of conservation of useful wild plants: An operational indicator for biodiversity
#' and sustainable development targets. Ecological Indicators. https://doi.org/10.1016/j.ecolind.2018.11.016
#'
#' @export

fcs_combine <- function(species,Workspace,run_version) {

  #importFrom("methods", "as")
  #importFrom("stats", "complete.cases", "filter", "median")
  #importFrom("utils", "data", "memory.limit", "read.csv", "write.csv")

  sp_dir <- paste0(Workspace,"/gap_analysis","/",species,"/",run_version)

    #in-situ and ex-situ summary files
  file_in <- paste0(sp_dir,"/gap_analysis/insitu/summary.csv")
  file_ex <- paste0(sp_dir,"/gap_analysis/exsitu/summary.csv")

  #read data from in-situ and ex-situ files
  data_in <- read.csv(file_in, sep=",", header=T)
  data_ex <- read.csv(file_ex, sep=",", header=T)

  #compute FCSc_min and FCSc_max
  data_comb <- data.frame(ID=species, FCSex=data_ex$FCS, FCSin=data_in$FCS)
  data_comb$FCSc_min <- min(c(data_ex$FCS,data_in$FCS),na.rm=T)
  data_comb$FCSc_max <- max(c(data_ex$FCS,data_in$FCS),na.rm=T)
  data_comb$FCSc_mean <- mean(c(data_ex$FCS,data_in$FCS),na.rm=T)

  #assign classes (min)
  if (data_comb$FCSc_min < 25) {
    data_comb$FCSc_min_class <- "HP"
  } else if (data_comb$FCSc_min >= 25 & data_comb$FCSc_min < 50) {
    data_comb$FCSc_min_class <- "MP"
  } else if (data_comb$FCSc_min >= 50 & data_comb$FCSc_min < 75) {
    data_comb$FCSc_min_class <- "LP"
  } else {
    data_comb$FCSc_min_class <- "SC"
  }

  #assign classes (max)
  if (data_comb$FCSc_max < 25) {
    data_comb$FCSc_max_class <- "HP"
  } else if (data_comb$FCSc_max >= 25 & data_comb$FCSc_max < 50) {
    data_comb$FCSc_max_class <- "MP"
  } else if (data_comb$FCSc_max >= 50 & data_comb$FCSc_max < 75) {
    data_comb$FCSc_max_class <- "LP"
  } else {
    data_comb$FCSc_max_class <- "SC"
  }

  #assign classes (mean)
  if (data_comb$FCSc_mean < 25) {
    data_comb$FCSc_mean_class <- "HP"
  } else if (data_comb$FCSc_mean >= 25 & data_comb$FCSc_mean < 50) {
    data_comb$FCSc_mean_class <- "MP"
  } else if (data_comb$FCSc_mean >= 50 & data_comb$FCSc_mean < 75) {
    data_comb$FCSc_mean_class <- "LP"
  } else {
    data_comb$FCSc_mean_class <- "SC"
  }

  #create output directory if it doesnt exist
  comb_dir <- paste0(sp_dir,"/gap_analysis/combined")
  if (!file.exists(comb_dir)) {dir.create(comb_dir)}

  #save output file and return
  write.csv(data_comb, paste(comb_dir,"/fcs_combined.csv",sep=""), row.names=F)
  return(data_comb)
}

# Workspace = "E:/CIAT/workspace/Workspace_test/workspace"
# run_version="v1"
# species_list <- c(
#   "Cucurbita_cordata",
#   "Cucurbita_digitata",
#   "Cucurbita_foetidissima",
#   "Cucurbita_palmata"
# )
# run_version <-"v1"
#
# lapply(1:length(species_list),function(i){
#   species <- species_list[[i]]
#   x <- fcs_combine(species,Workspace,run_version)
#   cat(paste0(species," DONE!"),"\n")
# })
