#
# load("data/CucurbitaData.rda")
#
#
# csv <- CucurbitaData
# names(csv)<- c("species","latitude","longitude","type")
#
#
# values <- checkOccurrences(csv)

checkOccurrences <- function(csv, removeDuplicated = FALSE){

  # check column names
  ## if don't match error out
  ## provide a message of the require
  requiredCols <- c("species","latitude","longitude","type")
  # select columns of interest
  missing_columns <- setdiff(requiredCols, names(csv))
  if(length(missing_columns) == 0){
    df <- csv |>
      dplyr::mutate(index = dplyr::row_number())|>
      dplyr::select(index, dplyr::all_of(requiredCols))
  }else{
    stop(paste("The following columns are missing ",
               missing_columns,
               ". Please add in order to move forward. Theres are case sensitive"))
  }

  # change the data type of the columns
  colTypes <- lapply(df, class)
  # check and change columns as needed
  if(class(colTypes$species) != "character"){
    df$species <- as.character(df$species)
    message(paste("Changed the data type from ", colTypes$species,
                  " to the required character" ))
  }
  if(class(colTypes$latitude) != "character"){
    df$latitude <- as.numeric(df$latitude)
    message(paste("Changed the data type from ", colTypes$latitude,
                  " to the required character" ))
  }
  if(class(colTypes$longitude) != "character"){
    df$longitude <- as.numeric(df$longitude)
    message(paste("Changed the data type from ", colTypes$longitude,
                  " to the required character" ))
  }
  if(class(colTypes$type) != "character"){
    df$type <- as.character(df$type)
    message(paste("Changed the data type from ", colTypes$type,
                  " to the required character" ))
  }

  # check the coordinate range (between -90:90 and -180:180)
  valid_lat <- df$latitude >= -90 & df$latitude <= 90 & !is.na(df$latitude)
  valid_lon <- df$longitude >= -180 & df$longitude <= 180 & !is.na(df$longitude)
  # combine to a single feature
  combined_vec <- ifelse(valid_lat == FALSE | valid_lon == FALSE, FALSE, TRUE)
  # index the records
  size <- nrow(df)
  # Remove rows with missing coordinates
  df <- df[combined_vec, ]
  invalidCoords <- size - nrow(df)
  # message
  message(paste("Removed  ", invalidCoords,
                " records because lat values were outside the range of -90-90 ",
                "or lonitude values were outside the range of -180-180,
                or there is a no value present for latitude or longitude for a record."))


  # Check for duplicated records (all rows)
  if(removeDuplicated == TRUE){
    dups <- duplicated(df[, requiredCols])
    nrows <- nrows(dups)
    df <- df[dups, ]
    removedDups <- size - nrow(df)
    # message
    message(paste("Removed  ", removedDups,
                  " records because of exact match of all records in the species, latitude, longitude, and type columns"))
  }

  # generate a plot of the points for a quality check
  points <- terra::vect(df, geom = c("longitude", "latitude"), crs =  "EPSG:4326")
  terra::plot(points,  main = "Quality Points for Gap Analysis",
              xlab = "Longitude", ylab = "Latitude")
  # return
  return(df)
}
