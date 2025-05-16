
## meant to replicated the user experience where they are bringing in there point data and rasters
# Load libraries
pacman::p_load(dplyr, terra, sf, leaflet,htmltools)

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

r1 <- terra::rast("~/trueNAS/work/cwr_wildgrapes/data/geospatial_datasets/protectedLands/wdpa_rasterized_all.tif")
# Start the single species workflow  --------------------------------------
taxon <- unique(occurrence_Data$species)[2]
sdm <- terra::unwrap(CucurbitaRasts)$digitata
# reclassify the values in the sdm to NA and 0
sdm <- terra::subst(sdm, 0, NA)

# run the srsex on the raw occurrence data to include all records
# regardless of presence of lat lon
srsex <- SRSex(taxon = taxon, occurrence_Data = occurrence_Data)

# run checks on the inputs
## points
occurrences <- checkOccurrences(csv = occurrence_Data, taxon = taxon)
## sdm

### add a
sdm <- checkRaster(sdm)
## protected area
proArea <- checkProtectAreas(proArea = proAreas, sdm = sdm)
## ecoregion check
eco <- checkEcoregion(eco = ecos, sdm = sdm, uniqueID ="ECO_ID_U" )

# generate gbuffer objects
## gBuffers
gbuffers <- generateGBuffers(taxon = taxon,
                             occurrence_Data = occurrences$data,
                             bufferDistM = 50000)

### Exsitu
#grs
grsex <- GRSex(taxon = taxon,
               sdm = sdm,
               gBuffer = gbuffers$data)
#ers
ersex <- ERSex(taxon = taxon,
               sdm = sdm,
               occurrence_Data = occurrences$data ,
               gBuffer = gbuffers$data,
               ecoregions = ecos,
               idColumn = "ECO_CODE" )
# fcsex
fcsex <- FCSex(taxon = taxon, srsex = srsex, grsex = grsex, ersex = ersex)

### Insitu
# srs
### not liking this map at all.... please take a look as it confusing why there is not
### direct overlap between the points and the SDM. I'm guessing this is just a image scale
### reduction issue but if feel more confusing then helpful at this point.
### we represent this metric different in other work but it'll take some effort to reproduce
srsin <- SRSin(taxon = taxon,
               sdm = sdm,
               occurrence_Data = occurrences$data,
               protectedAreas = proAreas)
# grs
grsin <- GRSin(taxon = taxon,
               sdm = sdm,
               protectedAreas  = proAreas)
# ers
ersin <- ERSin(taxon = taxon,
               sdm = sdm,
               occurrence_Data = occurrences$data,
               protectedAreas = proAreas,
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

