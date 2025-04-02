
## meant to replicated the user experience where they are bringing in there point data and rasters
# Load libraries
pacman::p_load(dplyr, terra, sf)

##Obtaining occurrences from example
load("data/CucurbitaData.rda")
##Obtaining Raster_list
load("data/CucurbitaRasts.rda")
##Obtaining protected areas raster
load("data/protectAreasRast.rda")
## ecoregion features
load("data/ecoExample.rda")


# load in some functions
files <- list.files("R",
                    pattern = ".R",
                    full.names = TRUE)
# source
for(i in files){
  print(i)
  source(i)}

# check points -- optional
dim(occurrence_Data)
data <- checkOccurrences(csv = occurrence_Data)
dim(data)

# check rasters -- optional
CucurbitaRasts
rasters <- checkRaster(raster = CucurbitaRasts)
rasters

# download data or set required files
ecos <- terra::vect(eco1) # currently a sf object see if it runs or change
proAreas <- terra::unwrap(protectAreasRast)



# Start the single species workflow  --------------------------------------

taxon <- unique(data$species)[2]
sdm <- rasters$digitata

# gBuffers
gbuffers <- generateGBuffers(taxon = taxon, occurrence_Data = data, bufferDistM = 50000)

### Exsitu
#srs - this can run with cleaned data which will remove all non lat lon points or the original data
srsex <- SRSex(taxon = taxon, occurrence_Data = occurrence_Data)
### lets no even show the running this on the quality check data --- because we need the full sample
srsexSpatial <- SRSex(taxon = taxon, occurrence_Data = data)
#grs
grsex <- GRSex(taxon = taxon, sdm = sdm, gBuffer = gbuffers)
#ers
ersex <- ERSex(taxon = taxon,
               sdm = sdm,
               occurrence_Data = data,
               gBuffer = gbuffers,
               ecoregions = ecos,
               idColumn = "ECO_CODE" )
# fcsex
fcsex <- FCSex(taxon = taxon, srsex = srsex,grsex = grsex, ersex = ersex)

### Insitu
# srs
srsin <- SRSin(taxon = taxon,
               sdm = sdm,
               occurrence_Data = data,
               protected_Areas = proAreas)
# grs
grsin <- GRSin(taxon = taxon,
               sdm = sdm,
               protected_Areas = proAreas)
# ers
ersin <- ERSin(taxon = taxon,
               sdm = sdm,
               occurrence_Data = data,
               protected_Areas = proAreas,
               ecoregions = ecos,
               idColumn = "ECO_CODE")
# fcs
fcsin <- FCSin(taxon = taxon,
               srsin = srsin,
               grsin = grsin,
               ersin = ersin)

### final score
fcs_combined <- FCSc_mean(taxon = taxon,
                          fcsin = fcsin,
                          fcsex = fcsex)



# custom input data  ------------------------------------------------------
ecos <- terra::vect("testData/us_eco_l3.shp") |>terra::makeValid()
allData <- read.csv("testData/allVitisData.csv")|>
  dplyr::select(species = "taxon",
                "latitude",
                "longitude",
                "type")
data <- checkOccurrences(allData)
sdm <- terra::rast("testData/Vitis acerifolia/prj_threshold.tif")
proArea <- terra::rast("testData/wdpa_reclass.tif") |>
  terra::crop(sdm)
taxon <- unique(data$species)[15]

# gBuffers
gbuffers <- generateGBuffers(taxon = taxon, occurrence_Data = data, bufferDistM = 50000)

### Exsitu
#srs - this can run with cleaned data which will remove all non lat lon points or the original data
srsex <- SRSex(taxon = taxon,occurrence_Data = allData)
srsexSpatial <- SRSex(taxon = taxon,occurrence_Data = data)
#grs
### getting a higher value the in the vitis workflow. Make just buffers are being cropped
grsex <- GRSex(taxon = taxon, sdm = sdm, gBuffer = gbuffers)
#ers
# troubleshoot SpatVector must have polygon geometry
ersex <- ERSex(taxon = taxon,
               sdm = sdm,
               occurrence_Data = data,
               gBuffer = gbuffers,
               ecoregions = ecos,
               idColumn = "US_L3CODE" )
# fcsex
fcsex <- FCSex(taxon = taxon, srsex = srsex,grsex = grsex, ersex = ersex)

### Insitu
## need more troubleshooting
# srs
srsin <- SRSin(taxon = taxon,
               sdm = sdm,
               occurrence_Data = data,
               protected_Areas = proArea)
# grs
grsin <- GRSin(taxon = taxon,
               sdm = sdm,
               protected_Areas = proArea)
# ers
ersin <- ERSin(taxon = taxon,
               sdm = sdm,
               occurrence_Data = data,
               protected_Areas = proArea,
               ecoregions = ecos,
               idColumn = "ECO_CODE")
# fcs
fcsin <- FCSin(taxon = taxon,
               srsin = srsin,
               grsin = grsin,
               ersin = ersin)

### final score
fcs_combined <- FCSc_mean(taxon = taxon,
                          fcsin = fcsin,
                          fcsex = fcsex)

