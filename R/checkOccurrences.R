
#'
#' @title Quality check on occurrences data
#' @name checkOccurrences
#' @description
#' Checks the column names, column data types, valid lat lon, and can optionally remove any duplicated lat lon records per species.
#' The cleaned and formated dataframe is returned as well as a map object show a quick reference of the points in space.
#'
#' @param csv A dataframe holding the occurrence data
#' @param taxon A character object that defines the name of the species as listed in the occurrence dataset
#' @param removeDuplicated : Binary parameter. TRUE == duplication values are remove. Set to FALSE as default
#'
#' @return A list object containing
#' 1. data : a data frames of values of occurrence data in the required format
#' 2. map : a leaflet object showing the spatial results of the function
#'
#'
#'

#' @examples
#' ##Obtaining occurrences from example
#' load("data/CucurbitaData.rda")
#'
#' # convert the dataset for function
#' taxon <- "Cucurbita_cordata"
#' occurrenceData <- CucurbitaData
#' #Running checkOccurrences
#' occurrences <- checkOccurrences(csv = occurrenceData
#'                     taxon = taxon,
#'                     removeDuplicated = FALSE
#'                     )
#'
#'
#'
#' @references
#' Khoury et al. (2019) Ecological Indicators 98:420-429. doi: 10.1016/j.ecolind.2018.11.016
#' Carver et al. (2021) GapAnalysis: an R package to calculate conservation indicators using spatial information
checkOccurrences <- function(csv, taxon, removeDuplicated = FALSE){

  # check column names
  ## if don't match error out
  ## provide a message of the require
  requiredCols <- c("species","latitude","longitude","type")
  # select columns of interest
  missing_columns <- setdiff(requiredCols, names(csv))
  if(length(missing_columns) == 0){
    csv <- csv |>
      dplyr::mutate(index = dplyr::row_number())|>
      dplyr::select(index, dplyr::all_of(requiredCols))
  }else{
    stop(paste("The following columns are missing ",
               missing_columns,
               ". Please add in order to move forward. Theres are case sensitive"))
  }

  # subset to the species of interest
  totalRows <- nrow(csv)
  df <- csv[csv$species == taxon, ]
  message(paste("A total of ", nrow(df),
                " out of the ", totalRows, "contained records for ", taxon))


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
                " records because lat values were outside the range of -90 to 90 or lonitude values were outside the range of -180-180",
                "or there is a no value present for latitude or longitude for a record."))


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
  # terra::plot(points,  main = "Quality Points for Gap Analysis",
  #             xlab = "Longitude", ylab = "Latitude")


  map_title <- "<h3 style='text-align:center; background-color:rgba(255,255,255,0.7); padding:2px;'>Quality Points for Gap Analysis</h3>"
  df$color <- ifelse(df$type == "G", yes = "#6300f0", no = "#1184d4")
  # Create the Leaflet map
  map <- leaflet(data = df) %>%
    addTiles() %>%  # Adds default OpenStreetMap map tiles
    addCircleMarkers(
      lng = ~longitude,
      lat = ~latitude,
      popup = ~paste("Point type:", ~type), # Optional: add popups
      radius = 5,          # Adjust marker size
      stroke = FALSE,
      color = ~color,
      fillOpacity = 0.8
    ) |>
    addControl(html = map_title, position = "bottomleft") #

  message(paste("All checks completed"))

  # return
  return(list(data = df,
         map = map))
}
