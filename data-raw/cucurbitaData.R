CucurbitaData <- utils::read.csv("data-raw/fileData/occurrenceData/cucurbitaData.csv",header = T)
usethis::use_data(CucurbitaData, overwrite = TRUE,compress = "bzip2")
