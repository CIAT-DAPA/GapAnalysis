
## meant to replicated the user experience where they are bringing in there point data and rasters
# Load libraries
pacman::p_load(dplyr, terra, sf)

# custom input data  ------------------------------------------------------
ecos <- terra::vect("testData/us_eco_l3.shp") |>terra::makeValid()
allData <- read.csv("testData/allVitisData.csv")|>
  dplyr::select(species = "taxon",
                "latitude",
                "longitude",
                "type")
proArea <- terra::rast("testData/wdpa_reclass.tif")

# load in some functions
files <- list.files("R",
                    pattern = ".R",
                    full.names = TRUE)
# source
for(i in files){
  print(i)
  source(i)}


# Start the single species workflow  --------------------------------------
sdm <- terra::rast("testData/Vitis acerifolia/prj_threshold.tif")
taxon <- unique(allData$species)[1]
# reclassify the values in the sdm to NA and 0
sdm <- subst(sdm, 0, NA)

# run the srsex on the raw occurrence data to include all records regardless of presence of lat lon
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
               ecoregions = eco,
               idColumn = "US_L3CODE" )
# fcsex
fcsex <- FCSex(taxon = taxon, srsex = srsex, grsex = grsex, ersex = ersex)

### Insitu
# srs
srsin <- SRSin(taxon = taxon,
               sdm = sdm,
               occurrence_Data = data,
               protectedAreas = proArea)
# grs
grsin <- GRSin(taxon = taxon,
               sdm = sdm,
               protectedAreas  = proArea)
# ers
ersin <- ERSin(taxon = taxon,
               sdm = sdm,
               occurrence_Data = data,
               protectedAreas = proArea,
               ecoregions = eco,
               idColumn = "US_L3CODE")
# fcs
fcsin <- FCSin(taxon = taxon,
               srsin = srsin,
               grsin = grsin,
               ersin = ersin)

### final score
fcs_combined <- FCSc_mean(taxon = taxon,
                          fcsin = fcsin,
                          fcsex = fcsex)





