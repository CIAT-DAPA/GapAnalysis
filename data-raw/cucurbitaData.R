cucurbitaData <- utils::read.csv("data-raw/fileData/occurrenceData/cucurbitaData.csv",header = T)
usethis::use_data(cucurbitaData, overwrite = TRUE,compress = "bzip2")
