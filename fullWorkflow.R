
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

# download data or set required files
ecos <- terra::vect(eco1) # currently a sf object see if it runs or change
proAreas <- terra::unwrap(protectAreasRast)


# Start the single species workflow  --------------------------------------

taxon <- unique(occurrence_Data$species)[2]
sdm <- raster$digitata
# reclassify the values in the sdm to NA and 0
sdm <- subst(sdm, 0, NA)

# run the srsex on the raw occurrence data to include all records regardless of presence of lat lon
srsex <- SRSex(taxon = taxon, occurrence_Data = occurrence_Data)

# run checks on the inputs
## points
data <- checkOccurrences(csv = occurrence_Data, taxon = taxon)
## sdm
sdm <- checkRaster(sdm)
## protected area
proArea <- checkProtectAreas(proArea = proAreas, sdm = sdm)
## ecoregion check
eco <- checkEcoregion(eco = ecos, sdm = sdm, uniqueID ="ECO_ID_U" )

# generate gbuffer objects
## gBuffers
gbuffers <- generateGBuffers(taxon = taxon,
                             occurrence_Data = data,
                             bufferDistM = 50000)

### Exsitu
#grs
grsex <- GRSex(taxon = taxon,
               sdm = sdm,
               gBuffer = gbuffers)
#ers
ersex <- ERSex(taxon = taxon,
               sdm = sdm,
               occurrence_Data = data,
               gBuffer = gbuffers,
               ecoregions = ecos,
               idColumn = "ECO_CODE" )
# fcsex
fcsex <- FCSex(taxon = taxon, srsex = srsex, grsex = grsex, ersex = ersex)

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
sdm <- terra::rast("testData/Vitis acerifolia/prj_threshold.tif")
sdm <- subst(sdm, 0, NA)
proArea <- terra::rast("testData/wdpa_reclass.tif")
taxon <- unique(allData$species)[1]

# srsex
srsex <- SRSex(taxon = taxon, occurrence_Data = allData)

# run checks on the inputs
## points
data <- checkOccurrences(csv = allData, taxon = taxon)
## sdm
sdm <- checkRaster(sdm)
## protected area
proArea <- checkProtectAreas(proArea = proArea, sdm = sdm)
## ecoregion check
eco <- checkEcoregion(eco = ecos, sdm = sdm, uniqueID ="US_L3CODE" )



# gBuffers
gbuffers <- generateGBuffers(taxon = taxon, occurrence_Data = data, bufferDistM = 50000)

### Exsitu
#grs
### getting a higher value the in the vitis workflow. Make just buffers are being cropped
grsex <- GRSex(taxon = taxon, sdm = sdm, gBuffer = gbuffers)
#ers
# troubleshoot SpatVector must have polygon geometry
ersex <- ERSex(taxon = taxon,
               sdm = sdm,
               occurrence_Data = data,
               gBuffer = gbuffers,
               ecoregions = eco,
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

