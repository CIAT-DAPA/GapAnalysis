############### SINGLE SPECIES WORKED EXAMPLE for README ##################################

# Load libraries
pacman::p_load(dplyr, terra, sf)

##Obtaining occurrences from example
load("data/CucurbitaData.rda")
##Obtaining Raster_list
load("data/CucurbitaRasts.rda")
##Obtaining protected areas raster
load("data/protectAreasRast.rda")
## ecoregions
load("data/ecoExample.rda")


# source in all function in a single location
## just a bit more a standard practice
functions <- list.files("R", pattern = ".R",full.names = TRUE) # |> lapply(source) # simple when everything works "_"
for(i in functions){
  print(i)
  source(i)
}
## to make this work I had to comment out all the executable code above the function that used for testing.



# Prep for the function
taxon <- CucurbitaData$species[1]
sdm <- terra::unwrap(CucurbitaRasts)[[1]]
occurrence_Data <- CucurbitaData
ecoregions <- terra::vect(eco1)
gBuffer <- generateGBuffers(taxon = taxon,
                            occurrence_Data = occurrence_Data,
                            bufferDistM =  50000)

# Generate objects
srsex <- SRSex(taxon = taxon,
               occurrence_Data = occurrence_Data)
grsex <- GRSex(taxon = taxon,
               sdm = sdm,
               gBuffer = gBuffer)
ersex <- ERSex(taxon = taxon,
               sdm = sdm,
               occurrence_Data = occurrence_Data, gBuffer = gBuffer,
               ecoregions = ecoregions, idColumn = "ECO_ID_U")
fcsex <- FCSex(taxon = taxon,
               srsex = srsex,
               grsex = grsex,
               ersex = ersex)

# Protect areas
protectAreasRast <- terra::unwrap(protectAreasRast)

ersin <- ERSin(taxon = taxon,
               sdm = sdm,
               occurrence_Data = occurrence_Data,
               protected_Areas = protectAreasRast,
               ecoregions = ecoregions,
               idColumn = "ECO_ID_U")

# Print results
print(paste("Taxon:", taxon))
print(fcsex)
print(ersin)






####### VIGNETTE WITH MULTIPLE ITERATIONS ######################################

# Load packages
pacman::p_load(dplyr, terra, sf)

## Obtaining occurrences from example
load("data/CucurbitaData.rda")
## Obtaining Raster_list
load("data/CucurbitaRasts.rda")
## Obtaining protected areas raster
load("data/protectAreasRast.rda")
## ecoregions
load("data/ecoExample.rda")

# Prep list of species
taxa <- unique(CucurbitaData$species)

########################################################################
###############################################################################
### Method 1: Named List

### DC: this is great and I think it's probably the option that we want to lead with
### can you fill out the workflow so it includes the two additional Insitue functions and the fcsmean


# Create an empty named list to store results
results_list <- list()

# Run using a for loop
for (i in seq_along(taxa)) {
  taxon <- taxa[i]

  # Assign the data for the selected taxon
  sdm <- terra::unwrap(CucurbitaRasts)[[i]]
  occurrence_Data <- CucurbitaData[CucurbitaData$species == taxon, ]
  ecoregions <- terra::vect(eco1)
  protectAreasRast <- terra::unwrap(protectAreasRast)

  # Generate gBuffer
  gBuffer <- generateGBuffers(taxon = taxon, occurrence_Data = occurrence_Data, bufferDistM = 50000)

  # Ex-situ analysis
  srsex <- SRSex(taxon = taxon, occurrence_Data = occurrence_Data)
  grsex <- GRSex(taxon = taxon, sdm = sdm, gBuffer = gBuffer)
  ersex <- ERSex(taxon = taxon, sdm = sdm, occurrence_Data = occurrence_Data, gBuffer = gBuffer, ecoregions = ecoregions, idColumn = "ECO_ID_U")
  fcsex <- FCSex(taxon = taxon, srsex = srsex, grsex = grsex, ersex = ersex)

  # In-situ analysis
  srsin <- SRSin(taxon = taxon, sdm = sdm, occurrence_Data = occurrence_Data, protected_Areas = protectAreasRast)
  grsin <- GRSin(taxon = taxon, sdm = sdm, protected_Areas = protectAreasRast)
  ersin <- ERSin(taxon = taxon, sdm = sdm, occurrence_Data = occurrence_Data, protected_Areas = protectAreasRast, ecoregions = ecoregions, idColumn = "ECO_ID_U")
  fcsin <- FCSin(taxon = taxon, srsin = srsin, grsin = grsin, ersin = ersin)

  # FCSmean calculation
  fcsmean <- FCSc_mean(taxon = taxon, fcsin = fcsin, fcsex = fcsex)

  # Store results in the named list
  results_list[[taxon]] <- list(
    srsex = srsex, grsex = grsex, ersex = ersex, fcsex = fcsex,
    srsin = srsin, grsin = grsin, ersin = ersin, fcsin = fcsin, fcsmean = fcsmean
  )

  # Print results
  print(paste("Taxon:", taxon))
  print(srsex)
  print(grsex)
  print(ersex)
  print(fcsex)
  print(srsin)
  print(grsin)
  print(ersin)
  print(fcsin)
  print(fcsmean)
}




###############################################################################
### Method 2: Conditional Binding

## i like this better as a approach but the specific output is super helpful because column names are reused and will be changed from what is produced by
## the specific functions

# Create an empty dataframe to store results
results_df <- NULL

# Run using a for loop
for (i in seq_along(taxa)) {
  taxon <- taxa[i]

  # Assign the data for the selected taxon
  sdm <- terra::unwrap(CucurbitaRasts)[[i]]
  occurrence_Data <- CucurbitaData[CucurbitaData$species == taxon, ]
  ecoregions <- terra::vect(eco1)
  protectAreasRast <- terra::unwrap(protectAreasRast)

  # Generate gBuffer
  gBuffer <- generateGBuffers(taxon = taxon, occurrence_Data = occurrence_Data, bufferDistM = 50000)

  # Ex-situ analysis
  srsex <- SRSex(taxon = taxon, occurrence_Data = occurrence_Data)
  grsex <- GRSex(taxon = taxon, sdm = sdm, gBuffer = gBuffer)
  ersex <- ERSex(taxon = taxon, sdm = sdm, occurrence_Data = occurrence_Data, gBuffer = gBuffer, ecoregions = ecoregions, idColumn = "ECO_ID_U")
  fcsex <- FCSex(taxon = taxon, srsex = srsex, grsex = grsex, ersex = ersex)

  # In-situ analysis
  srsin <- SRSin(taxon = taxon, sdm = sdm, occurrence_Data = occurrence_Data, protected_Areas = protectAreasRast)
  grsin <- GRSin(taxon = taxon, sdm = sdm, protected_Areas = protectAreasRast)
  ersin <- ERSin(taxon = taxon, sdm = sdm, occurrence_Data = occurrence_Data, protected_Areas = protectAreasRast, ecoregions = ecoregions, idColumn = "ECO_ID_U")
  fcsin <- FCSin(taxon = taxon, srsin = srsin, grsin = grsin, ersin = ersin)

  # FCSmean calculation
  fcsmean <- FCSc_mean(taxon = taxon, fcsin = fcsin, fcsex = fcsex)

  # Store results using conditional binding
  if (is.null(results_df)) {
    results_df <- data.frame(
      taxon = taxon,
      srsex = srsex, grsex = grsex, ersex = ersex, fcsex = fcsex,
      srsin = srsin, grsin = grsin, ersin = ersin, fcsin = fcsin, fcsmean = fcsmean
    )
  } else {
    results_df <- bind_rows(
      results_df,
      data.frame(
        taxon = taxon,
        srsex = srsex, grsex = grsex, ersex = ersex, fcsex = fcsex,
        srsin = srsin, grsin = grsin, ersin = ersin, fcsin = fcsin, fcsmean = fcsmean
      )
    )
  }

  # Print results
  print(paste("Taxon:", taxon))
  print(srsex)
  print(grsex)
  print(ersex)
  print(fcsex)
  print(srsin)
  print(grsin)
  print(ersin)
  print(fcsin)
  print(fcsmean)
}





###############################################################################
### Method 3: purrr::map2

# Load package
library(purrr)

# Prep list of species
speciesList <- unique(CucurbitaData$species)
sdms <- lapply(1:length(speciesList), function(i) terra::unwrap(CucurbitaRasts)[[i]])

## altering to use the name raster list indexing as a input and producing a specific function
## before calling purrr



# Load package
library(purrr)

# Prep list of species
speciesList <- unique(CucurbitaData$species)

# Prep raster datasets to ensure the correct one is selected
sdms <- terra::unwrap(CucurbitaRasts)
# Match the order of the SDMs to the order of the species in the taxa object
sdmList <- list(
  sdms$cordata,
  sdms$digitata,
  sdms$palmata
)
names(sdmList) <- taxa

# Define the workflow function
purrrWorkflow <- function(taxon, sdms) {
  sdm <- sdms[taxon][[1]] # Extract the raster from the list object

  # Assign the data for the selected taxon
  occurrence_Data <- CucurbitaData[CucurbitaData$species == taxon, ]
  ecoregions <- terra::vect(eco1)
  protectAreasRast <- terra::unwrap(protectAreasRast)

  # Generate gBuffer
  gBuffer <- generateGBuffers(taxon = taxon, occurrence_Data = occurrence_Data, bufferDistM = 50000)

  # Ex-situ analysis
  srsex <- SRSex(taxon = taxon, occurrence_Data = occurrence_Data)
  grsex <- GRSex(taxon = taxon, sdm = sdm, gBuffer = gBuffer)
  ersex <- ERSex(taxon = taxon, sdm = sdm, occurrence_Data = occurrence_Data, gBuffer = gBuffer, ecoregions = ecoregions, idColumn = "ECO_ID_U")
  fcsex <- FCSex(taxon = taxon, srsex = srsex, grsex = grsex, ersex = ersex)

  # In-situ analysis
  srsin <- SRSin(taxon = taxon, sdm = sdm, occurrence_Data = occurrence_Data, protected_Areas = protectAreasRast)
  grsin <- GRSin(taxon = taxon, sdm = sdm, protected_Areas = protectAreasRast)
  ersin <- ERSin(taxon = taxon, sdm = sdm, occurrence_Data = occurrence_Data, protected_Areas = protectAreasRast, ecoregions = ecoregions, idColumn = "ECO_ID_U")
  fcsin <- FCSin(taxon = taxon, srsin = srsin, grsin = grsin, ersin = ersin)

  # FCSmean calculation
  fcsmean <- FCSc_mean(taxon = taxon, fcsin = fcsin, fcsex = fcsex)

  # Return results as a list
  return(list(
    taxon = taxon,
    srsex = srsex, grsex = grsex, ersex = ersex, fcsex = fcsex,
    srsin = srsin, grsin = grsin, ersin = ersin, fcsin = fcsin, fcsmean = fcsmean
  ))
}

# Run the workflow using purrr::map
results <- purrr::map(.x = speciesList, .f = purrrWorkflow, sdms = sdmList)
names(results) <- speciesList

# Alternative: purrr::map2 (if using two separate lists like speciesList and sdms)
results <- purrr::map2(.x = speciesList, .y = sdmList, .f = function(taxon, sdm) {
  # Include the same logic as above
  occurrence_Data <- CucurbitaData[CucurbitaData$species == taxon, ]
  ecoregions <- terra::vect(eco1)
  protectAreasRast <- terra::unwrap(protectAreasRast)

  # Ex-situ and in-situ analyses as above
  gBuffer <- generateGBuffers(taxon = taxon, occurrence_Data = occurrence_Data, bufferDistM = 50000)
  srsex <- SRSex(taxon = taxon, occurrence_Data = occurrence_Data)
  grsex <- GRSex(taxon = taxon, sdm = sdm, gBuffer = gBuffer)
  ersex <- ERSex(taxon = taxon, sdm = sdm, occurrence_Data = occurrence_Data, gBuffer = gBuffer, ecoregions = ecoregions, idColumn = "ECO_ID_U")
  fcsex <- FCSex(taxon = taxon, srsex = srsex, grsex = grsex, ersex = ersex)

  srsin <- SRSin(taxon = taxon, sdm = sdm, occurrence_Data = occurrence_Data, protected_Areas = protectAreasRast)
  grsin <- GRSin(taxon = taxon, sdm = sdm, protected_Areas = protectAreasRast)
  ersin <- ERSin(taxon = taxon, sdm = sdm, occurrence_Data = occurrence_Data, protected_Areas = protectAreasRast, ecoregions = ecoregions, idColumn = "ECO_ID_U")
  fcsin <- FCSin(taxon = taxon, srsin = srsin, grsin = grsin, ersin = ersin)

  fcsmean <- FCSc_mean(taxon = taxon, fcsin = fcsin, fcsex = fcsex)

  return(list(
    taxon = taxon,
    srsex = srsex, grsex = grsex, ersex = ersex, fcsex = fcsex,
    srsin = srsin, grsin = grsin, ersin = ersin, fcsin = fcsin, fcsmean = fcsmean
  ))
})

# Display results
results



### this total works, option above just seems a bit clearer to me as there's less wrapped into the purrr functions .
# Run purrr::map2 function to generate metrics for all taxa
results <- purrr::map2(speciesList, sdms, ~ {
  taxon <- .x
  sdm <- .y

  # Assign the data for the selected taxon
  occurrence_Data <- CucurbitaData[CucurbitaData$species == taxon, ]
  ecoregions <- terra::vect(eco1)
  protectAreasRast <- terra::unwrap(protectAreasRast)

  # Generate gBuffer
  gBuffer <- generateGBuffers(taxon = taxon, occurrence_Data = occurrence_Data, bufferDistM = 50000)

  # Generate objects for each function
  srsex <- SRSex(taxon = taxon, occurrence_Data = occurrence_Data)
  grsex <- GRSex(taxon = taxon, sdm = sdm, gBuffer = gBuffer)
  ersex <- ERSex(taxon = taxon, sdm = sdm, occurrence_Data = occurrence_Data, gBuffer = gBuffer, ecoregions = ecoregions, idColumn = "ECO_ID_U")
  fcsex <- FCSex(taxon = taxon, srsex = srsex, grsex = grsex, ersex = ersex)
  ersin <- ERSin(taxon = taxon, sdm = sdm, occurrence_Data = occurrence_Data, protected_Areas = protectAreasRast, ecoregions = ecoregions, idColumn = "ECO_ID_U")

  list(taxon = taxon, srsex = srsex, grsex = grsex, ersex = ersex, fcsex = fcsex, ersin = ersin)
})

# Display results
results




###############################################################################
## SG drop??
### Method 4 : Pre-create a dataframe

# Create an empty dataframe to store results
### were building this out more so it's prepped to store the results Not sure important in this cause but you generally want to avoid
### adding onto dataframes within a for loop, it's best to build the dataframe of a specific size that you need before an and populate rows within the loop
results_df <- data.frame(matrix(nrow = length(taxa), ncol = 17))
names(results_df) <- c("Taxon",
                       "SRS exsitu","GRS exsitu","ERS exsitu","FCS exsitu","FCS existu score",
                       "SRS insitu","GRS insitu","ERS insitu","FCS insitu","FCS insitu score",
                       "FCSc_min","FCSc_max","FCSc_mean","FCSc_min_class",  "FCSc_max_class", "FCSc_mean_class")

# prep raster datasets to make sure the correct one is selected
sdms <- terra::unwrap(CucurbitaRasts)
# match the order of the SDMs to the order of the species in the taxa object
sdmList <- list(
  sdms$cordata,
  sdms$digitata,
  sdms$palmata
)
names(sdmList) <- taxa

# Run using a for loop
for (i in seq_along(taxa)) {
  taxon <- taxa[i]

  # Assign the data for the selected taxon
  sdm <- terra::unwrap(CucurbitaRasts)[[i]] ## using positional indexing here which is fine, but an easy source of error
  # my suggestion might be users organize there rasters before hand in a names and using character matching
  ## either option works but because the names list requires some up front work users are more likely to check that everything lines up before running the process.
  sdm <- sdmList[taxon][[1]] # extra indexing is needed her to get the raster out of the list object.


  occurrence_Data <- CucurbitaData[CucurbitaData$species == taxon, ]
  ecoregions <- terra::vect(eco1)
  protectAreasRast <- terra::unwrap(protectAreasRast)

  # Generate gBuffer
  gBuffer <- generateGBuffers(taxon = taxon, occurrence_Data = occurrence_Data, bufferDistM = 50000)

  # Generate objects for each function
  srsex <- SRSex(taxon = taxon, occurrence_Data = occurrence_Data)
  grsex <- GRSex(taxon = taxon, sdm = sdm, gBuffer = gBuffer)
  ersex <- ERSex(taxon = taxon, sdm = sdm, occurrence_Data = occurrence_Data, gBuffer = gBuffer, ecoregions = ecoregions, idColumn = "ECO_ID_U")

  # Generate FCSex
  fcsex <- FCSex(taxon = taxon, srsex = srsex, grsex = grsex, ersex = ersex)

  # In-situ analysis
  ## add the SRS, GRS, and FCSin scores
  srsin <- SRSin(taxon = taxon, sdm = sdm, occurrence_Data = occurrence_Data, protected_Areas = protectAreasRast)
  grsin <- GRSin(taxon = taxon, sdm = sdm, protected_Areas = protectAreasRast)
  ersin <- ERSin(taxon = taxon, sdm = sdm, occurrence_Data = occurrence_Data, protected_Areas = protectAreasRast, ecoregions = ecoregions, idColumn = "ECO_ID_U")

  # Generate FCSin
  fcsin <- FCSin(taxon = taxon, srsin = srsin, grsin = grsin, ersin = ersin)

  ## adding final score calculation
  # generating  FCSin
  fcsmean <- FCSc_mean(taxon = taxon, fcsin = fcsin, fcsex = fcsex)

  # Store results in the dataframe
  ### so here we can simplfy by using some of our fcs results,
  summary <- fcsex |>
    dplyr::left_join(fcsin, by = "Taxon")|>
    dplyr::left_join(fcsmean[,c(1,4:9)], by = "Taxon") |> # kinda sloppy with the positional indexing
    dplyr::select(names(results_df))
  # the other option here is explicitly define positiosn
  results_df[i, "Taxon"] <- taxon
  results_df[i, "SRS exsitu"] <- fcsex$`SRS exsitu`
  results_df[i, "GRS exsitu"] <- fcsex$`GRS exsitu`
  # or more  short hand
  results_df[i, "Taxon"] <- taxon
  results_df[i, 2:6] <- fcsex[,2:6]


  # results_df <- rbind(results_df, data.frame(taxon = taxon, srsex = srsex, grsex = grsex, ersex = ersex, fcsex = fcsex, ersin = ersin))

  # Print results
  print(paste("Taxon:", taxon))
  print(srsex)
  print(grsex)
  print(ersex)
  print(fcsex)
  print(ersin)

}




######## SG 2020_02_28


###
# I've started on this be decided to wait for the moment. I'll but putting some more
# time back into the vitis project soon and I think this comparision will be a little
# easier once those features are more fresh in the mind.
###





##################### PART II : Run with Vitis data 2.5 arc #########################

# Vitis arizonica 2.5 Arc Run with data exploration checks

# Load necessary libraries
pacman::p_load(dplyr, terra, sf)

# load the folder to the project file location for easier paths... please see the git ignore if you do something similar

# Load occurrence data as sf object
spatial_data_path <- "C:/Users/sgora/Desktop/Agrobiodiversity/Vitis/Data/data/v.arizonica_run20241204_2.5k/run20241204_2.5k/occurances/spatialData.gpkg"
spatial_data_path <- "run20241204/occurances/spatialData.gpkg"


occurrence_Data_sf <- sf::st_read(spatial_data_path)
print("Occurrence Data (sf object):")
#  Reading layer `spatialData' from data source
# `C:\Users\sgora\Desktop\Agrobiodiversity\Vitis\Data\data\v.arizonica_run20241204_2.5k\run20241204_2.5k\occurances\spatialData.gpkg'
#  using driver `GPKG'
# Simple feature collection with 1891 features and 26 fields
# Geometry type: POINT
# Dimension:     XY
# Bounding box:  xmin: -119.9071 ymin: 22.1167 xmax: -96.00398 ymax: 40.07268
# Geodetic CRS:  WGS 84


# Convert to data frame and make longitude and latitude columns readable
occurrence_Data <- data.frame(occurrence_Data_sf)
occurrence_Data$longitude <- sf::st_coordinates(occurrence_Data_sf)[, 1]
occurrence_Data$latitude <- sf::st_coordinates(occurrence_Data_sf)[, 2]
print("Occurrence Data (data frame):")
print(head(occurrence_Data))

# Rename columns to match the function's requirements
occurrence_Data <- dplyr::rename(occurrence_Data, species = taxon, Species = species)

# Check raw occurrence records before filtering
total_occurrences_raw <- nrow(occurrence_Data)
occurrences_with_latlong_raw <- nrow(occurrence_Data[!is.na(occurrence_Data$longitude) & !is.na(occurrence_Data$latitude), ])
germplasm_records_raw <- nrow(occurrence_Data[occurrence_Data$type == "G", ])
reference_records_raw <- nrow(occurrence_Data[occurrence_Data$type == "H", ])
unique_data_sources_raw <- length(unique(occurrence_Data$databaseSource))

cat("Total Occurrences (Raw):", total_occurrences_raw, "\n")
# Total Occurrences (Raw): 1891
cat("Occurrences with Lat/Long (Raw):", occurrences_with_latlong_raw, "\n")
# Occurrences with Lat/Long (Raw): 1891
cat("Germplasm Records (G) (Raw):", germplasm_records_raw, "\n")
# Germplasm Records (G) (Raw): 189
cat("Reference Records (H) (Raw):", reference_records_raw, "\n")
# Reference Records (H) (Raw): 1702
cat("Unique Data Sources (Raw):", unique_data_sources_raw, "\n")
# Unique Data Sources (Raw): 5


### not really a issue having the SRSex measures not lining up



# Filter for the Vitis arizonica
filtered_occurrence_Data <- occurrence_Data |>
  dplyr::filter(species == "Vitis arizonica" & type == "G")

# Check filtered occurrence records
total_occurrences_filtered <- nrow(filtered_occurrence_Data)
occurrences_with_latlong_filtered <- nrow(filtered_occurrence_Data[!is.na(filtered_occurrence_Data$longitude) & !is.na(filtered_occurrence_Data$latitude), ])
germplasm_records_filtered <- nrow(filtered_occurrence_Data[filtered_occurrence_Data$type == "G", ])
reference_records_filtered <- nrow(filtered_occurrence_Data[filtered_occurrence_Data$type == "H", ])
unique_data_sources_filtered <- length(unique(filtered_occurrence_Data$databaseSource))

cat("Total Occurrences (Filtered):", total_occurrences_filtered, "\n")
# Total Occurrences (Filtered): 189
cat("Occurrences with Lat/Long (Filtered):", occurrences_with_latlong_filtered, "\n")
# Occurrences with Lat/Long (Filtered): 189
cat("Germplasm Records (G) (Filtered):", germplasm_records_filtered, "\n")
# Germplasm Records (G) (Filtered): 189
cat("Reference Records (H) (Filtered):", reference_records_filtered, "\n")
# Reference Records (H) (Filtered): 0
cat("Unique Data Sources (Filtered):", unique_data_sources_filtered, "\n")
# Unique Data Sources (Filtered): 1


# Load protected areas raster data and set correct CRS for function
protected_area_raster_path <- "C:/Users/sgora/Desktop/Agrobiodiversity/Vitis/Data/data/wdpa_rasterized_all.tif"
protected_Areas <- terra::rast(protected_area_raster_path)
crs_wgs84 <- "+proj=longlat +datum=WGS84 +no_defs"
terra::crs(protected_Areas) <- crs_wgs84

# Load the species distribution model (SDM)
sdm_path <- "C:/Users/sgora/Desktop/Agrobiodiversity/Vitis/Data/data/v.arizonica_run20241204_2.5k/run20241204_2.5k/results/prj_threshold.tif"
sdm <- terra::rast(sdm_path)
terra::crs(sdm) <- crs_wgs84

# Crop and resample the protected areas raster to match the extent of the sdm
protected_Areas <- terra::crop(protected_Areas, terra::ext(sdm))
protected_Areas <- terra::resample(protected_Areas, sdm, method = "bilinear")

# Load ecoregions data as sf object and set CRS
ecoregions_data_path <- "C:/Users/sgora/Desktop/Agrobiodiversity/Vitis/Data/data/tnc_terr_ecoregions.gpkg"
ecoregions_Data_sf <- sf::st_read(ecoregions_data_path)
sf::st_crs(ecoregions_Data_sf) <- 4326
ecoregions_Data <- terra::vect(ecoregions_Data_sf)
terra::crs(ecoregions_Data) <- crs_wgs84
ecoregions_Data <- terra::makeValid(ecoregions_Data)

# Source functions
source("R/generateGBuffers.R")
source("R/ERSex.R")
source("R/SRSex.R")
source("R/GRSex.R")
source("R/FCSex.R")
source("R/ERSin.R")
source("R/SRSin.R")
source("R/GRSin.R")

# Define the list of species
taxa <- list("Vitis arizonica")  # Add more species as needed

# Create an empty dataframe to store results
results_df <- data.frame()

# Create lists to store intermediate results
intermediate_results <- list()

# Iterate over each species in the taxa list
for (i in seq_along(taxa)) {
  taxon <- taxa[[i]]

  # Filter for the species of interest
  filtered_occurrence_Data <- occurrence_Data |>
    dplyr::filter(species == taxon & type == "G")

  # Debugging print
  print(paste("Filtered data for taxon:", taxon))
  print(head(filtered_occurrence_Data))

  # Check if filtered_occurrence_Data is not empty
  if (nrow(filtered_occurrence_Data) == 0) {
    warning(paste("No occurrence data for taxon:", taxon))
    next
  }

  # Save intermediate occurrence data
  intermediate_results[[taxon]] <- list(filtered_occurrence_Data = filtered_occurrence_Data)

  # Convert filtered occurrence data to SpatVector with CRS
  d1 <- terra::vect(filtered_occurrence_Data, geom=c("longitude", "latitude"), crs=crs_wgs84)
  d1 <- terra::makeValid(d1)

  # Generate buffer
  bufferDistM <- 50000
  gBuffer <- generateGBuffers(taxon = taxon,
                              occurrence_Data = filtered_occurrence_Data,
                              bufferDistM = bufferDistM)

  # Save intermediate buffer result
  intermediate_results[[taxon]]$gBuffer <- gBuffer

  # Generate objects for each function
  srsex <- SRSex(taxon = taxon, occurrence_Data = occurrence_Data)
  grsex <- GRSex(taxon = taxon, sdm = sdm, gBuffer = gBuffer)
  ersex <- ERSex(taxon = taxon, sdm = sdm, occurrence_Data = occurrence_Data, gBuffer = gBuffer, ecoregions = ecoregions_Data, idColumn = "ECO_ID_U")

  # Save intermediate results
  intermediate_results[[taxon]]$srsex <- srsex
  intermediate_results[[taxon]]$grsex <- grsex
  intermediate_results[[taxon]]$ersex <- ersex

  # Generate final object for FCSex function
  fcsex <- FCSex(taxon = taxon, srsex = srsex, grsex = grsex, ersex = ersex)

  # Run the ERSin function
  ersin <- ERSin(taxon = taxon, sdm = sdm, occurrence_Data = filtered_occurrence_Data, protected_Areas = protected_Areas, ecoregions = ecoregions_Data, idColumn = "ECO_ID_U")

  # Save remaining intermediate results
  intermediate_results[[taxon]]$fcsex <- fcsex
  intermediate_results[[taxon]]$ersin <- ersin

  # Store final results in the dataframe
  results_df <- rbind(results_df, data.frame(taxon = taxon, srsex = srsex, grsex = grsex, ersex = ersex, fcsex = fcsex, ersin = ersin))

  # Print results
  print(paste("Taxon:", taxon))
  print(fcsex)
  print(ersin)
}


print(head(filtered_occurrence_Data))
print(gBuffer)


write.xlsx(srsex, "varizonica_1k_srsex.xlsx", overwrite = TRUE)
write.xlsx(grsex, "varizonica_1k_grsex.xlsx", overwrite = TRUE)
write.xlsx(ersex, "varizonica_1k_ersex.xlsx", overwrite = TRUE)
write.xlsx(fcsex, "varizonica_1k_fcsex.xlsx", overwrite = TRUE)
write.xlsx(ersin, "varizonica_1k_ersin.xlsx", overwrite = TRUE)




##################### PART III: Run with different data sources #########################
# gather 2 ecoregion files and test results









