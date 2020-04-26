cucurbitaData <- utils::read.csv("data-raw/exampleData/occurrenceData/cucurbitaData.csv",header = T)
usethis::use_data(cucurbitaData, overwrite = TRUE,compress = "bzip2")
