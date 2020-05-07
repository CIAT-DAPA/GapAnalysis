# GapAnalysis R package

## Description
The GapAnalysis R package provides a series of functions that allows the user to evaluate the ex situ and in situ conservation status of taxa in a standardized and reproducible way, to combine these metrics into an integrated conservation assessment, and to compare and calculate a conservation indicator metric across taxa. GapAnalysis generates quantitative metrics and spatial outputs which represent the state of conservation and where gaps in the collection/conservation of taxa exist on the landscape. The methods are fully described in Ramirez-Villegas et al. (2010) and Khoury et al. (2019) (see reference list).

The GapAnalysis functions requires the user to provide two inputs
A `data.frame` of species occurrences
A `raster` object of the predicted potential habitat (species distribution model)

This library consists of 16 functions within 4 families: pre-analysis, ex situ conservation gap analysis, in situ conservation gap analysis, and summary evaluations. In short, the pre-analysis process establishes the file structure and prepares the input data. The ex situ and in situ processes perform the respective conservation strategy gap analyses and produce both quantitative and spatial results. The combined assessment merges the individual assessments, summarizes the results across taxa, calculates the indicator, and generates a summary html document for each taxon, which can be used to evaluate outputs and aid conservation planning. 

As with any evaluation the quality and completeness of the data used in the assessment is the limiting factor to the overall usefulness of the metric. A full description of the application of this process can be found here (link to the paper eventually)

## Installation
GapAnalysis can be installed as follows
```r
library(devtools)
install_github("ccsosa/GapAnalysis")
```
A full list of libraries needed for the package is included below.

```r
### this is not complete but I figure it is a reasonable placeholder for the time being.  
suppressMessages(require(tmap))
suppressMessages(require(raster))
suppressMessages(require(maptools))
suppressMessages(require(rgdal))
suppressMessages(require(ff))
suppressMessages(require(ffbase))
suppressMessages(require(sf))
suppressMessages(require(dplyr))
```

## Usage
We provide the below reproducible example (also available in the package documentation)

```r
##Load package
library(GapAnalysis)

##Obtaining occurrences from example
data(CucurbitaData)

##Obtaining species names from the data
speciesList <- unique(CucurbitaData$taxon)

##Obtaining raster_list
data(CucurbitaRasters)
CucurbitaRasters <- raster::unstack(CucurbitaRasters)

##Obtaining protected areas raster
data(ProtectedAreas)

##Obtaining ecoregions shapefile
data(ecoregions)

#Running all three Ex situ gap analysis steps using insituGapAnalysis function
exsituGapMetrics <- ExsituCompile(species_list=speciesList,
                                      occurrenceData=CucurbitaData,
                                      raster_list=CucurbitaRasters,
                                      bufferDistance=50000,
                                      ecoReg=ecoregions)

#Running all three In situ gap analysis steps using insituGapAnalysis function
insituGapMetrics <- InsituCompile(species_list=speciesList,
                                       occurrenceData=CucurbitaData,
                                       raster_list=CucurbitaRasters,
                                       proArea=ProtectedAreas,
                                       ecoReg=ecoregions)

## Obtaining AOO and EOO ##
eooAoo_table <- GapAnalysis::eooAoo(species_list = speciesList,
                               occurrenceData = CucurbitaData)

## Combine gap analysis metrics
fcsCombine <- FCSc_mean(fcsEx = exsituGapMetrics,fcsIn = insituGapMetrics)

## Generate summary HTML file with all result
summaryHTML_file <- summary_HTML(species_list=speciesList,
                                 occurrenceData = CucurbitaData,
                                 raster_List=CucurbitaRasters,
                                 proArea=ProtectedArea,
                                 bufferDistance=50000,
                                 insituSummary=insituGapMetrics,
                                 exsituSummary=exsituGapMetrics,
                                 fcsSummary=fcsCombine,
                                 eooAooSummary=eooAoo_table,
                                 outputFolder=".",
                                 writeRasters=F)
```

The below sub-sections provide further details on the input data and GapAnalysis steps.

### Data inputs
**_Species occurrences_**

A `data.frame` of species occurrences and record type. This process can handle single or multiple taxa.

taxon | longitude | latitude | type
------------ | ------------- | -------------| -------------
Cucurbita_cordata | -113.563 | 28.9457 | G
Cucurbita_digitata |  | | H

**Taxon:** this value will be the key for all functions in this library. Ensure it is consistent for all records and is included in the file name of your predicted potential habitat .tif as well.

**longitude** and **latitude** must be in decimal degrees, preferably with the highest accuracy possible.

**Type:** All records must be classified as either a reference observation (typically the main presence data input into the species distribution modeling, labeled H as most records in our previous research source from herbaria), or as a “site of collection” location of an existing ex situ accession from a conservation repository (labeled G, as most records in our previous research source from genebanks). This distinction is significant for multiple evaluations and effort must be taken to ensure the correct assignment of these values.

Digital repositories such as GBIF, EDDmaps, and IDIGBIO contain observed locations of a taxon considered “H” type occurrences in GapAnalyis. From our experience there can be duplication within and between such databases and care should be taken to reduce duplication where possible.

The major sources for G occurrence data that the authors have used in GapAnalysis include [USDA GRIN](https://npgsweb.ars-grin.gov/gringlobal/search.aspx), [GENESYS](https://www.genesys-pgr.org/), [FAO WIEWS](http://www.fao.org/wiews/en/), [PlantSearch](https://tools.bgci.org/plant_search.php), and [GBIF](https://www.gbif.org/) (records designated as 'living specimen'). Generally, data from these sources would be considered a G type if it is still an active and living accession. Duplication between these sources may exist.

More information and examples of how to make the distinction between “H” and “G” points can be found [here](https://doi.org/10.1111/DDI.13008).


**_Predicted Potential Habitat_**

The `raster` representing the predicted potential extent of suitable habitat is used by multiple functions to represent the maximum potential range of a species. This is then compared to what is conserved _ex situ_ or _in situ_. Although a required input, the generation of species distribution models is not included in GapAnalysis because a number of R packages for this process already exist (packages `sdm`, `wallace`, `dismo` and `maxnet`).


### Workflow
The recommended workflow is as follows.

**Pre-analysis**
 - `GetDatasets` downloads the protected areas and ecoregions datasets from our data repository
 - `OccurrenceCounts` creates a .csv file with counts of G, H, and those record types with coordinates for all taxa, based on input occurrence data
 - `.Gbuffer` is an internal function that creates a circular buffer of user-defined size (default is 0.5 degrees, equivalent to ca. 50 km radius) around each G point for each taxon, which represents the geographic areas already considered to be sufficiently collected for ex situ conservation. The output of this process is a raster. Since this is not an exported function to use it you will need to type `GapAnalysis:::.Gbuffer` in R.

**Ex-situ Analysis**
 - `SRSex` calculates the Sampling Representativeness Score for _ex situ_ conservation
 - `GRSex` calculates the Geographic Representativeness Score for _ex situ_ conservation
 - `ERSex` calculates the Ecological Representativeness Score for _ex situ_ conservation
 - `FCSex` calculates the Final Conservation Score for _ex situ_ conservation as an average of the above 3 scores
 - `ExsituCompile` compiles the 4 _ex situ_ conservation metrics

**In-situ Analysis**
 - `SRSin` calculates the Sampling Representativeness Score for _in situ_ conservation
 - `GRSin` calculates the Geographic Representativeness Score for _in situ_ conservation
 - `ERSin` calculates the Ecological Representativeness Score for _in situ_ conservation
 - `FCSin` calculates the Final Conservation Score for _in situ_ conservation as an average of the above 3 scores
 - `InsituCompile` ccompiles the 4 _in situ_ conservation metrics

**Summary evaluations**   
 - `FCSc_mean` computes the mean as well as minimum and maximum of the _ex situ_ and _in situ_ Final Conservation Scores. It also assigns taxa to priority categories based on these final scores (high priority (HP) for further conservation action assigned when FCS < 25, medium priority (MP) where 25 ≤ FCS < 50, low priority (LP) where 50 ≤ FCS < 75, and sufficiently conserved (SC) for taxa whose FCS ≥75)
- `indicator` calculates an indicator across assessed taxa, which can be applied at national, regional, global, or any other scale (Khoury et al., 2019). The indicator is calculated separately with regard to ex situ, in situ, and combined conservation, by deriving the proportion of taxa categorized as SC or LP out of all taxa.
 - `SummaryHTML` produces a summary HTML output with taxon specific results. Alongside quantitative results, these include taxon-level “gap” maps. With regard to ex situ conservation, the ex situ geographic gap map is created by subtracting the G buffered areas out of the distribution model of each taxon, leaving only those areas considered not sufficiently sampled for ex situ conservation. The ex situ ecological gap map is created by mapping only the spatial areas within the distribution model of each taxon which are occupied by ecoregions not represented by G buffers. With regard to in situ conservation, the in situ geographic gap map is created by subtracting the protected areas out of the distribution model of each taxon, revealing those areas in the model not currently in protected areas. The in situ ecological gap map is created by mapping only the spatial areas within the distribution model of each taxon which are occupied by ecoregions not represented at all in protected areas. These four maps are embedded in the html document, and are also written as raster files.

Each function can be run as a standalone method and in any order. However, we recommend following this workflow as it will ensure dependencies for individual functions are in place and that the variables are stored correctly to successfully produce the final summary document. For more details on each of these calculations, see the list of references below.

## Authors
Main: Daniel Carver, Chrystian C. Sosa, Colin K. Khoury, and Julian Ramirez-Villegas
Other contributors: Harold A. Achicanoy, Maria Victoria Diaz, Steven Sotelo, Nora P. Castaneda-Alvarez

## References

Ramirez-Villegas J, Khoury CK, Jarvis A, Debouck DG, Guarino L (2010) A gap analysis methodology for collecting crop genepools: a case study with Phaseolus beans. PLoS One 5, e13497. [doi:10.1371/journal.pone.0013497](http://dx.doi.org/10.1371%2Fjournal.pone.0013497)

Khoury CK, Amariles D, Soto JS, Diaz MV, Sotelo S, Sosa CC, Ramirez-Villegas J, Achicanoy HA, Velásquez-Tibata J, Guarino L, Leon B, Navarro-Racines C, Castañeda-Álvarez NP, Dempewolf H, Wiersema JH, and Jarvis A (2019) Comprehensiveness of conservation of useful wild plants: an operational indicator for biodiversity and sustainable development targets. Ecological Indicators 98: 420-429. doi: [10.1016/j.ecolind.2018.11.016](https://doi.org/10.1016/j.ecolind.2018.11.016)

Castañeda-Álvarez NP*, Khoury CK*, Achicanoy HA, Bernau V, Dempewolf H, Eastwood RJ, Guarino L, Harker RH, Jarvis A, Maxted N, Mueller JV, Ramirez-Villegas J, Sosa CC, Struik PC, Vincent H, and Toll J (2016) Global conservation priorities for crop wild relatives. Nature Plants 2(4): 16022. doi: [10.1038/nplants.2016.22](http://www.nature.com/articles/nplants201622)

## License
GNU GENERAL PUBLIC LICENSE Version 3
