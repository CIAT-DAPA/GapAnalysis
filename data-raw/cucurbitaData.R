setwd("~/Repositories/GapAnalysis")
CucurbitaData <- utils::read.csv("data-raw/fileData/OccurrenceData/CucurbitaData.csv",header = T)
CucurbitaData <- as.data.frame(CucurbitaData)
usethis::use_data(CucurbitaData, overwrite = TRUE,compress = "bzip2")
