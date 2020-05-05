pkgname <- "GapAnalysis"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('GapAnalysis')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("ERSex")
### * ERSex

flush(stderr()); flush(stdout())

### Name: ERSex
### Title: Environmental representativeness score estimation (Ex-situ
###   conservation)
### Aliases: ERSex

### ** Examples

##Obtaining occurrences from example
data(cucurbitaData)
speciesList <- unique(cucurbitaData$taxon)
## Obtaining rasterList object. ##
data(cucurbitaRasters)
cucurbitaRasters <- raster::unstack(cucurbitaRasters)
##Obtaining ecoregions shapefile
data(ecoregions)

ERSex_df <- ERSex(species_list = speciesList,
                    occurrenceData = cucurbitaData,
                    raster_list = cucurbitaRasters,
                    bufferDistance = 50000,
                    ecoReg=ecoregions)




cleanEx()
nameEx("ERSin")
### * ERSin

flush(stderr()); flush(stdout())

### Name: ERSin
### Title: Environmental representativeness score estimation (In-situ
###   conservation)
### Aliases: ERSin

### ** Examples

##Obtaining occurrences from example
data(cucurbitaData)
##Obtaining species names from the data
speciesList <- unique(cucurbitaData$taxon)
##Obtaining raster_list
data(cucurbitaRasters)
cucurbitaRasters <- raster::unstack(cucurbitaRasters)
##Obtaining protected areas raster
data(protectedAreas)
##Obtaining ecoregions shapefile
data(ecoregions)

ERSin_df <- ERSin(species_list = speciesList,
                   occurrenceData = cucurbitaData,
                   raster_list = cucurbitaRasters,
                   proArea= protectedAreas,
                   ecoReg=ecoregions)




cleanEx()
nameEx("ExsituCompile")
### * ExsituCompile

flush(stderr()); flush(stdout())

### Name: ExsituCompile
### Title: Ex-situ gap analysis calculation (Ex-situ conservation)
### Aliases: ExsituCompile

### ** Examples

##Obtaining occurrences from example
data(cucurbitaData)
##Obtaining species names from the data
speciesList <- unique(cucurbitaData$taxon)
##Obtaining raster_list
data(cucurbitaRasters)
cucurbitaRasters <- raster::unstack(cucurbitaRasters)
##Obtaining ecoregions shapefile
data(ecoregions)

#Running all three Ex situ gap analysis steps using ExsituCompile function
exsituGapMetrics <- ExsituCompile(species_list=speciesList,
                                      occurrenceData=cucurbitaData,
                                      raster_list=cucurbitaRasters,
                                      bufferDistance=50000,
                                      ecoReg=ecoregions)




cleanEx()
nameEx("FCSc_mean")
### * FCSc_mean

flush(stderr()); flush(stdout())

### Name: FCSc_mean
### Title: Combining Ex-situ and In-situ gap analysis results in one
###   comprehensive conservation score (Summary Assessments)
### Aliases: FCSc_mean

### ** Examples

##Obtaining occurrences from example
data(cucurbitaData)
##Obtaining species names from the data
speciesList <- unique(cucurbitaData$taxon)
##Obtaining raster_list
data(cucurbitaRasters)
cucurbitaRasters <- raster::unstack(cucurbitaRasters)
##Obtaining protected areas raster
data(protectedAreas)
##Obtaining ecoregions shapefile
data(ecoregions)

#Running all three Ex-situ gap analysis steps using ExsituCompile function
exsituGapMetrics <- ExsituCompile(species_list=speciesList,
                                      occurrenceData=cucurbitaData,
                                      raster_list=cucurbitaRasters,
                                      bufferDistance=50000,
                                      ecoReg=ecoregions)


#Running all three In-situ gap analysis steps using InsituCompile function
insituGapMetrics <- InsituCompile(species_list=speciesList,
                                       occurrenceData=cucurbitaData,
                                       raster_list=cucurbitaRasters,
                                       proArea=protectedAreas,
                                       ecoReg=ecoregions)

fcsCombine <- FCSc_mean(fcsEx = exsituGapMetrics,fcsIn = insituGapMetrics)




cleanEx()
nameEx("FCSex")
### * FCSex

flush(stderr()); flush(stdout())

### Name: FCSex
### Title: Final ex situ conservation score estimation (Ex-situ
###   conservation)
### Aliases: FCSex

### ** Examples

##Obtaining occurrences from example
data(cucurbitaData)
##Obtaining species names from the data
speciesList <- unique(cucurbitaData$taxon)
##Obtaining raster_list
data(cucurbitaRasters)
cucurbitaRasters <- raster::unstack(cucurbitaRasters)
##Obtaining ecoregions shapefile
data(ecoregions)

#Calculating SRSex value
SRSex_df <- SRSex(species_list = speciesList,
                    occurrenceData = cucurbitaData)

#Calculating GRSex value
GRSex_df <- GRSex(species_list = speciesList,
                    occurrenceData = cucurbitaData,
                    raster_list = cucurbitaRasters)

#Calculating ERSex value
ERSex_df <- ERSex(species_list = speciesList,
                    occurrenceData = cucurbitaData,
                    raster_list = cucurbitaRasters,
                    bufferDistance = 50000,
                    ecoReg=ecoregions
                    )

#Calculating final conservation for ex-situ gap analysis

FCSex_df <- FCSex(srsDF = SRSex_df, grsDF = GRSex_df, ersDF = ERSex_df)




cleanEx()
nameEx("FCSin")
### * FCSin

flush(stderr()); flush(stdout())

### Name: FCSin
### Title: Final in-situ conservation score estimation (In-situ
###   conservation)
### Aliases: FCSin

### ** Examples

##Obtaining occurrences from example
data(cucurbitaData)
##Obtaining species names from the data
speciesList <- unique(cucurbitaData$taxon)
##Obtaining raster_list
data(cucurbitaRasters)
cucurbitaRasters <- raster::unstack(cucurbitaRasters)
##Obtaining protected areas raster
data(protectedAreas)
##Obtaining ecoregions shapefile
data(ecoregions)
#Calculating SRSin value
SRSin_df <- SRSin(species_list = speciesList,
                    occurrenceData = cucurbitaData,
                    raster_list = cucurbitaRasters,
                    proArea=protectedAreas)

#Calculating GRSin value
GRSin_df <- GRSin(species_list = speciesList,
                    occurrenceData = cucurbitaData,
                    raster_list = cucurbitaRasters,
                    proArea=protectedAreas)

#Calculating ERSin value
ERSin_df <- ERSin(species_list = speciesList,
                    occurrenceData = cucurbitaData,
                    raster_list = cucurbitaRasters,
                    proArea=protectedAreas,
                    ecoReg=ecoregions)

#Calculating final conservation for ex-situ gap analysis

FCSin_df <- FCSin(srsDF = SRSin_df, grsDF = GRSin_df, ersDF = ERSin_df)




cleanEx()
nameEx("GRSex")
### * GRSex

flush(stderr()); flush(stdout())

### Name: GRSex
### Title: Geographical representativeness score estimation (Ex-situ
###   conservation)
### Aliases: GRSex

### ** Examples

##Obtaining occurrences from example
data(cucurbitaData)
speciesList <- unique(cucurbitaData$taxon)
## Obtaining rasterList objet. ##
data(cucurbitaRasters)
cucurbitaRasters <- raster::unstack(cucurbitaRasters)
#Calculating GRSex value
GRSex_df <- GRSex(species_list = speciesList,
                    occurrenceData = cucurbitaData,
                    raster_list = cucurbitaRasters,
                    bufferDistance = 50000)




cleanEx()
nameEx("GRSin")
### * GRSin

flush(stderr()); flush(stdout())

### Name: GRSin
### Title: Geographical representativeness score estimation (In-situ
###   conservation)
### Aliases: GRSin

### ** Examples

##Obtaining occurrences from example
data(cucurbitaData)
##Obtaining species names from the data
speciesList <- unique(cucurbitaData$taxon)
##Obtaining raster_list
data(cucurbitaRasters)
cucurbitaRasters <- raster::unstack(cucurbitaRasters)
##Obtaining protected areas raster
data(protectedAreas)

GRSin_df <- GRSin(species_list = speciesList,
                    occurrenceData = cucurbitaData,
                    raster_list = cucurbitaRasters,
                    proArea=protectedAreas)





cleanEx()
nameEx("Gbuffer")
### * Gbuffer

flush(stderr()); flush(stdout())

### Name: Gbuffer
### Title: @title Geodesic buffer around points (long, lat) using metric
###   radius EXTRACTED FROM GEOBUFFER R PACKAGE
### Aliases: Gbuffer
### Keywords: internal

### ** Examples

## Not run: 
##D bucharest_500km <- geobuffer_pts(xy = data.frame(lon = 26.101390,
##D                                                  lat = 44.427764),
##D                                  dist_m = 500*10^3,
##D                                  output = "sf")
##D bucharest_500km
##D plot(bucharest_500km)
##D 
##D library(mapview)
##D library(sf)
##D mapView(as(bucharest_500km, "Spatial"), alpha.regions = 0.2)
## End(Not run)



cleanEx()
nameEx("GetDatasets")
### * GetDatasets

flush(stderr()); flush(stdout())

### Name: GetDatasets
### Title: Preparing datasets to run GapAnalysis functions
### Aliases: GetDatasets

### ** Examples

## Not run: 
##D GetDatasets()
## End(Not run)




cleanEx()
nameEx("InsituCompile")
### * InsituCompile

flush(stderr()); flush(stdout())

### Name: InsituCompile
### Title: In-situ gap analysis calculation (In-situ conservation)
### Aliases: InsituCompile

### ** Examples

##Obtaining occurrences from example
data(cucurbitaData)
##Obtaining species names from the data
speciesList <- unique(cucurbitaData$taxon)
##Obtaining raster_list
data(cucurbitaRasters)
cucurbitaRasters <- raster::unstack(cucurbitaRasters)
##Obtaining protected areas raster
data(protectedAreas)
##Obtaining ecoregions shapefile
data(ecoregions)

#Running all three In situ gap analysis steps using InsituCompile function
insituGapMetrics <- InsituCompile(species_list=speciesList,
                                       occurrenceData=cucurbitaData,
                                       raster_list=cucurbitaRasters,
                                       proArea=protectedAreas,
                                       ecoReg=ecoregions)




cleanEx()
nameEx("OccurrenceCounts")
### * OccurrenceCounts

flush(stderr()); flush(stdout())

### Name: OccurrenceCounts
### Title: Generating Species Count Data Frame
### Aliases: OccurrenceCounts

### ** Examples

data(cucurbitaData)
##Obtaining species names from the data
speciesList <- unique(cucurbitaData$taxon)
countDF <- OccurrenceCounts(speciesList[[1]],cucurbitaData)




cleanEx()
nameEx("SRSex")
### * SRSex

flush(stderr()); flush(stdout())

### Name: SRSex
### Title: Sample representativeness score estimation (Ex-situ
###   conservation)
### Aliases: SRSex

### ** Examples

##Obtaining occurrences from example
data(cucurbitaData)
##Obtaining species names from the data
speciesList <- unique(cucurbitaData$taxon)
SRSex_df <- SRSex(species_list = speciesList,
                    occurrenceData = cucurbitaData)




cleanEx()
nameEx("SRSin")
### * SRSin

flush(stderr()); flush(stdout())

### Name: SRSin
### Title: Sample representativeness score estimation (In-situ
###   conservation)
### Aliases: SRSin

### ** Examples

##Obtaining occurrences from example
data(cucurbitaData)
##Obtaining species names from the data
speciesList <- unique(cucurbitaData$taxon)
##Obtaining raster_list
data(cucurbitaRasters)
cucurbitaRasters <- raster::unstack(cucurbitaRasters)
##Obtaining protected areas raster
data(protectedAreas)
SRSin_df <- SRSin(species_list = speciesList,
                    occurrenceData = cucurbitaData,
                    raster_list=cucurbitaRasters,
                    proArea=protectedAreas)




cleanEx()
nameEx("eooAoo")
### * eooAoo

flush(stderr()); flush(stdout())

### Name: eooAoo
### Title: IUCN conservations status using Area of occupancy (AOO) and
###   extent of occurrence (EOO) (IUCN Redlist)
### Aliases: eooAoo

### ** Examples

data(cucurbitaData)
##Obtaining species names from the data
speciesList <- unique(cucurbitaData$taxon)
## Obtaining AOO and EOO ##
eooAoo <- GapAnalysis::eooAoo(species_list = speciesList,
                               occurrenceData = cucurbitaData)




cleanEx()
nameEx("summary_HTML")
### * summary_HTML

flush(stderr()); flush(stdout())

### Name: summary_HTML
### Title: Creating a summary HTML document for gap analysis
### Aliases: summary_HTML

### ** Examples

## Not run: 
##D ##Obtaining occurrences from example
##D data(cucurbitaData)
##D ##Obtaining species names from the data
##D speciesList <- unique(cucurbitaData$taxon)
##D ##Obtaining raster_list
##D data(cucurbitaRasters)
##D cucurbitaRasters <- raster::unstack(cucurbitaRasters)
##D ##Obtaining protected areas raster
##D data(protectedAreas)
##D ##Obtaining ecoregions shapefile
##D data(ecoregions)
##D 
##D #Running all three Ex situ gap analysis steps using insituGapAnalysis function
##D exsituGapMetrics <- ExsituCompile(species_list=speciesList,
##D                                       occurrenceData=cucurbitaData,
##D                                       raster_list=cucurbitaRasters,
##D                                       bufferDistance=50000,
##D                                       ecoReg=ecoregions)
##D 
##D 
##D #Running all three In situ gap analysis steps using insituGapAnalysis function
##D insituGapMetrics <- InsituCompile(species_list=speciesList,
##D                                        occurrenceData=cucurbitaData,
##D                                        raster_list=cucurbitaRasters,
##D                                        proArea=protectedAreas,
##D                                        ecoReg=ecoregions)
##D 
##D ## Obtaining AOO and EOO ##
##D eooAoo_table <- GapAnalysis::eooAoo(species_list = speciesList,
##D                                occurrenceData = cucurbitaData)
##D 
##D fcsCombine <- FCSc_mean(fcsEx = exsituGapMetrics,fcsIn = insituGapMetrics)
##D 
##D summaryHTML_file <- summary_HTML(species_list=speciesList,
##D                                  occurrenceData = cucurbitaData,
##D                                  raster_List=cucurbitaRasters,
##D                                  proArea=protectedArea,
##D                                  bufferDistance=50000,
##D                                  insituSummary=insituGapMetrics,
##D                                  exsituSummary=exsituGapMetrics,
##D                                  fcsSummary=fcsCombine,
##D                                  eooAooSummary=eooAoo_table,
##D                                  outputFolder=".",
##D                                  writeRasters=F)
## End(Not run)




### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
