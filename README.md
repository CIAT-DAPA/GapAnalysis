# GapAnalysis R package

document updated 2025-09 

## Version 2 Changes 
Conceptually the results and methods of the gap analysis approach have not changed with the second versions of the tool. This current release integrates the 
[terra](https://rspatial.github.io/terra/index.html) spatial data libraries. Also, additional map visualizations are provided
as objects with the outputs from the gap analysis metric functions (ex. ERSex, GRSin). 
More additions are expected in the future based on end user requests and feedback.



## Description
The GapAnalysis R package evaluates the ex situ and in situ conservation status of taxa, combines these metrics into an integrated  assessment, and calculates an indicator metric across taxa. GapAnalysis generates quantitative and spatial outputs which demonstrate the state of conservation as well as where gaps in protection exist. The methods are fully described in Carver et al. (2021). Articles by Ramirez-Villegas et al. (2010), Castañeda-Álvarez and Khoury et al. (2016), and Khoury et al. (2019a, b; 2020) describe the main steps toward the current methodology. 

The GapAnalysis functions require the user to provide two inputs: a `data.frame` of species occurrences, and a `rast` object of the predicted habitat (species distribution model) for each assessed taxon.

This library consists of 17 functions within 3 families: data checks and gathering, ex situ conservation gap analysi and in situ conservation gap analysis. In short, the data checks and gather process establishes gives the option to download protected areas and ecoregion datasets or enables users to run quality checks on their own data.
The ex situ and in situ processes perform the respective conservation strategy gap analyses and produce both quantitative and spatial results.

## Installation
GapAnalysis can be installed as follows
```r
#CRAN
install.packages("GapAnalysis")
#Alternative: GitHub
library(devtools)
remotes::install_github("CIAT-DAPA/GapAnalysis")
```

A full list of libraries needed for the package is included below.
**Depends:** 
  R (>= 4.3.0),
**Imports:**
	  magrittr,
    dataverse,
    dplyr,
    leaflet,
    terra


## Usage
We provide the below reproducible example (also available in the package documentation).
Please note this example is provided at 10 arc minutes resolution for efficient processing time; the results in the associated published article (Khoury et al. 2019c) and described in the associated R package article (Carver et al. 2021) differ as the analysis was conducted at 2.5 arc minutes resolution. For more details on accessing and utilizing the 2.5 arc minutes dataset see [ecoregions and protected areas](#ecolink).




```r
##Load package
library(GapAnalysis)

##Obtaining occurrences from example
data(CucurbitaData)

##Obtaining Raster_list
data(CucurbitaRasts)

##Obtaining protected areas raster
data(ProtectedAreas)
## ecoregion features
data(ecoregions)

# convert the dataset for function
taxon <- "Cucurbita_cordata"
sdm <- terra::unwrap(CucurbitaRasts)$cordata
occurrenceData <- CucurbitaData
protectedAreas <- terra::unwrap(ProtectedAreas)
ecoregions <- terra::vect(ecoregions)

# generate exsitu conservation summaries
## sample representativeness score exsitu 
srs_exsitu <- SRSex(taxon = taxon,
                occurrence_Data  = CucurbitaData)

## Generate buffer objects        
gBuffer <- generateGBuffers(taxon = taxon,
 occurrenceData = occurrenceData,
 bufferDistM = 50000 )

## geographic representativeness score  exsitu
grs_exsitu <- GRSex(taxon = taxon,
 sdm = sdm,
 gBuffer = gBuffer )

## Ecological representativeness score exsitu 
ers_exsitu <- ERSex(taxon = taxon,
 sdm = sdm,
 occurrence_Data = occurrenceData,
 gBuffer = gBuffer,
 ecoregions = ecoregions,
 idColumn = "ECO_NAME" )
 
# Running final conservation score exsitu 
fcs_exsitu <- FCSex(taxon = taxon,
 srsex = srs_exsitu,
 grsex = grs_exsitu,
 ersex = ers_exsitu
 )
 
# generate insitu conservation summaries
## sample representativeness score insitu
srs_insitu <- SRSin(taxon = taxon,
 sdm = sdm,
 occurrenceData = CucurbitaData,
 protectedAreas = protectedAreas)
 
## Geographic representativeness score insitu
 grs_insitu <- GRSin(taxon = taxon,
   sdm = sdm,
   protectedAreas = protectedAreas)
  
## ecological representativeness score insitu 
ers_insitu <- ERSin(taxon = taxon,
 sdm = sdm,
 occurrenceData = occurrenceData,
 protectedAreas = protectedAreas,
 ecoregions = ecoregions,
 idColumn = "ECO_NAME" )

## final representativeness score insitu 
fcs_insitu <- FCSin(taxon = taxon,
 srsin = srs_insitu,
 grsin = grs_insitu,
 ersin = ers_insitu
 )
## combine conservation score 
fsc_combine <- FCSc_mean(taxon = taxon,
 fcsin = fcs_insitu,
 fcsex = fcs_exsitu)
 
```

For an example with mutliple species see the file `multipleSpecies_vignette.RMD` 


The below sub-sections provide further details on the input data and GapAnalysis steps.

### Data inputs
**_Species occurrences_**

A `data.frame` of species occurrences and record type. This process can handle single or multiple taxa.

species | latitude | longitude | type
------------ | ------------- | -------------| -------------
Cucurbita_cordata | 28.9457 | -113.563 | G
Cucurbita_digitata |  | | H

**species:** this value will be the key for all functions in this library. Ensure it is consistent for all records and is included in the file name of your predicted potential habitat `raster` as well.

**latitude** and **longitude** must be in decimal degrees, preferably with the highest accuracy possible.

**type:** All records must be classified as either a reference observation (typically the main presence data input into the species distribution modeling, labeled H as most records in our previous research source from herbaria), or as a “site of collection” location of an existing ex situ accession from a conservation repository (labeled G, as most records in our previous research source from genebanks). This distinction is significant for multiple evaluations and effort must be taken to ensure the correct assignment of these values.

Digital repositories such as GBIF, EDDmaps, and IDIGBIO contain observed locations of a taxon considered “H” type occurrences in GapAnalysis. From our experience there can be duplication within and between such databases and care should be taken to reduce duplication where possible.

The major sources for G occurrence data that the authors have used in GapAnalysis include [USDA GRIN](https://npgsweb.ars-grin.gov/gringlobal/search.aspx), [GENESYS](https://www.genesys-pgr.org/), [FAO WIEWS](http://www.fao.org/wiews/en/), [PlantSearch](https://tools.bgci.org/plant_search.php), and [GBIF](https://www.gbif.org/) (records designated as 'living specimen'). Generally, data from these sources would be considered a G type if it is still an active and living accession. Duplication between these sources may exist.

More information and examples of how to make the distinction between “H” and “G” points can be found [here](https://doi.org/10.1111/DDI.13008).

<a name="ecolink">
<b><i>Ecoregions and Protected Area </b></i>
</a>
The ecoregion and protected areas datasets are provide through the package via the `GetDatasets()` functions. The files will be downloaded and store at

```r
system.file("data/preloaded_data/ecoRegion/tnc_terr_ecoregions.shp",package = "GapAnalysis")
```

These files can be found accessed directly at the [Dataverse repository](https://dataverse.harvard.edu/dataverse/GapAnalysis) associated with this package.
The original datasets can be found here [ecoregions](http://maps.tnc.org/gis_data.html), 
[world database of protected areas](https://www.protectedplanet.net/en/thematic-areas/wdpa?tab=WDPA)). The ecoregion dataset is provided in its native vector data type. The package's WDPA layer has been transformed from a vector to a binary raster at 2.5 arc minutes resolution raster.

**_Predicted Habitat_**

The `rast` representing the predicted extent of suitable habitat (species distribution model) is used by multiple functions to represent the maximum potential range of a taxon. This is then compared to what is conserved _ex situ_ or _in situ_. Although a required input, the generation of species distribution models is not included in GapAnalysis because a number of R packages for this process already exist (e.g. packages `sdm`, `wallace`, `dismo` and `maxnet`).


### Workflow
The recommended workflow is as follows:

**Pre-analysis**
 - `getDatasets` downloads the protected areas and ecoregions datasets from our data repository
 - `checkEcoregions` provides a quality check of the ecoregion file -- optional  
 - `checkRasters` provides a quality check of the sdm data -- recommended 
 - `checkProtectedAreas` provides a quality check of the protected areas file -- recommended 
 - `checkOccurrences` provides a quality check of the input species data -- recommended 

**Ex-situ Analysis**
 - `SRSex` calculates the Sampling Representativeness Score for _ex situ_ conservation
 - `GRSex` calculates the Geographic Representativeness Score for _ex situ_ conservation. During this process, an ex situ geographic gap map is also created for each species by subtracting the G buffered areas out of the distribution model of each taxon, leaving only those areas considered not sufficiently sampled for ex situ conservation
 - `ERSex` calculates the Ecological Representativeness Score for _ex situ_ conservation. During this process, an ex situ ecological gap map is also created for each species by mapping only the spatial areas within the distribution model of each taxon which are occupied by ecoregions not represented by G buffers
 - `FCSex` calculates the Final Conservation Score for _ex situ_ conservation as an average of the above 3 scores and assigns a priority category for each taxon based on the final conservation score

**In-situ Analysis**
 - `SRSin` calculates the Sampling Representativeness Score for _in situ_ conservation
 - `GRSin` calculates the Geographic Representativeness Score for _in situ_ conservation. During this process, an in situ geographic gap map is also created for each species by subtracting the protected areas out of the distribution model of each taxon, revealing those areas in the model not currently in protected areas
 - `ERSin` calculates the Ecological Representativeness Score for _in situ_ conservation. During this process, an in situ ecological gap map is also created for each species by by mapping only the spatial areas within the distribution model of each taxon which are occupied by ecoregions not represented at all in protected areas
 - `FCSin` calculates the Final Conservation Score for _in situ_ conservation as an average of the above 3 scores and assigns a priority category for each taxon based on the final conservation score

**Summary evaluations**   
 - `FCSc_mean` computes the mean as well as minimum and maximum of the _ex situ_ and _in situ_ Final Conservation Scores. It also assigns taxa to priority categories based on final conservation scores (high priority (HP) for further conservation action assigned when FCS < 25, medium priority (MP) where 25 ≤ FCS < 50, low priority (LP) where 50 ≤ FCS < 75, and sufficiently conserved (SC) for taxa whose FCS ≥75)
 
**Internal functions**
 - `generateCounts` creates a `data.frame` with counts of G, H, and those record types with coordinates for all taxa, based on input occurrence data
 - `generateEcoSelection` helper funtion utilized by both the ERSex and ERSin functions 
 - `generateGBuffers` produces buffer vect object of the G point features 
 
Each function can be run as a standalone method and in any order. However, we recommend following this workflow as it will ensure dependencies for individual functions are in place and that the variables are stored correctly to successfully produce the final summary document. For more details on each of these calculations, see the list of references below.

## Authors
Main: Daniel Carver, Chrystian C. Sosa, Sarah Gora,  Colin K. Khoury, and Julian Ramirez-Villegas

Other contributors: Harold A. Achicanoy, Maria Victoria Diaz, Steven Sotelo, Nora P. Castaneda-Alvarez

## References

Carver D, Sosa CC, Khoury CK, Achicanoy HA, Diaz MV, Sotelo S, Castañeda-Álvarez NP, and Ramírez-Villegas JR (2021) GapAnalysis: an R package to calculate conservation indicators using spatial information. Ecography. doi: 10.1111/ecog.05430. (https://doi.org/10.1111/ecog.05430)

Castañeda-Álvarez NP, Khoury CK, Achicanoy HA, Bernau V, Dempewolf H, Eastwood RJ, Guarino L, Harker RH, Jarvis A, Maxted N, Mueller JV, Ramirez-Villegas J, Sosa CC, Struik PC, Vincent H, and Toll J (2016) Global conservation priorities for crop wild relatives. Nature Plants 2(4): 16022. doi: [10.1038/nplants.2016.22](http://www.nature.com/articles/nplants201622)

Khoury CK, Amariles D, Soto JS, Diaz MV, Sotelo S, Sosa CC, Ramirez-Villegas J, Achicanoy HA, Velásquez-Tibata J, Guarino L, Leon B, Navarro-Racines C, Castañeda-Álvarez NP, Dempewolf H, Wiersema JH, and Jarvis A (2019a) Comprehensiveness of conservation of useful wild plants: an operational indicator for biodiversity and sustainable development targets. Ecological Indicators 98: 420-429. doi: [10.1016/j.ecolind.2018.11.016](https://doi.org/10.1016/j.ecolind.2018.11.016)

Khoury CK, Carver D, Barchenger DW, Barboza G, van Zonneweld M, Jarret R, Bohs L, Kantar MB, Uchanski M, Mercer K, Nabhan GP, Bosland PW, and Greene SL (2019b) Modeled distributions and conservation status of the wild relatives of chile peppers (Capsicum L). Diversity and Distributions 26(2): 209-225. doi: 10.1111/DDI.13008. https://doi.org/10.1111/DDI.13008

Khoury CK, Carver D, Greene SL, Williams KA, Achicanoy HA, Schori M, León B, Wiersema JH, and Frances A (2020) Crop wild relatives of the United States require urgent conservation action. Proc Natl Acad Sci USA 117(52): 33351-33357. doi: 10.1073/pnas.2007029117. https://doi.org/10.1073/pnas.2007029117

Khoury CK, Carver D, Kates HR, Achicanoy HA, van Zonneweld M, Thomas E, Heinitz C, Jarret R, Labate JA, Reitsma K, Nabhan GP, and Greene SL (2019c) Distributions, conservation status, and abiotic stress tolerance potential of wild cucurbits (Cucurbita L.). Plants, People, Planet 2(3): 269-283. doi: 10.1002/ppp3.10085. https://doi.org/10.1002/ppp3.10085

Ramirez-Villegas J, Khoury CK, Jarvis A, Debouck DG, Guarino L (2010) A gap analysis methodology for collecting crop genepools: a case study with Phaseolus beans. PLoS One 5, e13497. [doi:10.1371/journal.pone.0013497](http://dx.doi.org/10.1371%2Fjournal.pone.0013497)

## License
GNU GENERAL PUBLIC LICENSE Version 3
