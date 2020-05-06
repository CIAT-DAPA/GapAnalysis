# GapAnalysis R package

## Description
The GapAnalysis R package provides a series of functions that allows the user to evaluate the in situ and ex situ conservation status of a plant species in a standardized and reproducible way. This method will generate quantitative metrics and spatial outputs which represent the need for conservation and where gap in the collection/conservation of the species exist on the landscape.

The GapAnalysis functions requires the user to provide two inputs
A `data.frame` of species occurrences
A `raster` object of the predicted potential habitat

This library consists of 16 functions. These are split between 4 categories; pre-analysis, in-situ, ex-situ, and combined assessment. The pre-analysis steps establishes the data structure and cleans the data. The in-situ and ex-situ fuctions produce the quantitative conservation assessments.

While each step of the process can be ran as a stand alone function the library is intended to be used following the example workflow provided below.

As with any evaluation the quality and completeness of the data used in the assessment is the limiting factor to the overall usefulness of these metrics. A full description of the application of this process can be found here(link to the paper eventually)

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

The below sub-sections provide an explanation of data and individual gap analysis steps.

### Data inputs
**_Species occurrences_**

A `data.frame` of species occurrences and type. This process can handle single and multiple species.

taxon | longitude | latitude | type
------------ | ------------- | -------------| -------------
Cucurbita_cordata | -113.563 | 28.9457 | G
Cucurbita_digitata |  | | H

**Taxon:** this value will be the key for all functions in this library. Ensure it is consistent for all records and is included in the file name of your predicted potential habitat .tif as well.

**Type:** The type column refers to if the occurrence data represents a herbarium sample or a germplasm sample. Type H, for herbarium, is a known occurrence of the species in the landscape. Type G, for genebank, is a known occurrence of a species that has been collected and is stored as a living sample either in botanical gardens or genebanks.

This distinction is significant for multiple evaluations and effort must be taken to ensure the correct assignment of these values.

As of 2019, the major sources for Genebank occurrence data that the authors have evaluated include.
[USDA GRIN](https://npgsweb.ars-grin.gov/gringlobal/search.aspx)
[GENESYS](https://www.genesys-pgr.org/)
[WIEWS](http://www.fao.org/wiews/en/)

Generally, data from these sources would be considered a “G” type if it is still an active accession. Duplication between these source may exist.

Digital repositories such as GBIF, EDDmaps, and IDIGBIO, are most likely to contain observed locations of a species with no living sample. These would be considered “H” type occurrences. As these databases are not actively curated there is potential for duplicates between sources. Please evaluate this potential when gathering your data and assigning type.

Other independent sources of data need to be evaluated for type on a case by case basis.

More information and examples of how to make the distinction between “H” and “G” points can be found [here](link to either detailed tutorial or paper that defines these distinctions in more detail).


**_Predicted Potential Habitat_**

The `raster` representing the predicted potential extent of suitable habitat is used by multiple functions to represent the maximum potential range of a species. This is then compared to what is conserved _ex-situ_ or _in-situ_.

### Workflow
Main_code - example of the recommend workflow.

**Pre-analysis**
 - `GetDatasets` downloads the protected areas and ecoregions datasets from our data repository

**Ex-situ Analysis**
 - `SRSex` calculates the Sampling Representativeness Score for _ex-situ_ conservation
 - `ERSex` calculates the Environmental Representativeness Score for _ex-situ_ conservation
 - `GRSex` calculates the Geographic Representativeness Score for _ex-situ_ conservation
 - `FCSex` calculates the Final Conservation Score for _ex-situ_ conservation
 - `ExsituCompile` calculates all of the 4 metrics above at once for _ex-situ_ conservation

**In-situ Analysis**
 - `SRSin` calculates the Sampling Representativeness Score for _in-situ_ conservation
 - `ERSin` calculates the Environmental Representativeness Score for _in-situ_ conservation
 - `GRSin` calculates the Geographic Representativeness Score for _in-situ_ conservation
 - `FCSin` calculates the Final Conservation Score for _in-situ_ conservation
 - `InsituCompile` calculates all of the 4 metrics above at once for _in-situ_ conservation

**Summarize results**   
 - `FCSc_mean` computes the mean of the _ex-situ_ and _in-situ_ Final Conservation Scores
 - `eooAoo` uses the `redlistr` package to calculate the extent and area of ocupancy
 - `summary_HTML` produces a summary HTML output with all results

Each function can be run as a standalone method and in any order. However, we recommend following this workflow as it will ensure dependencies for individual functions are in place and that the variables are stored correctly to successfully produce the final summary document. For more details on each of these calculations, see the list of references below.

## Authors
Main: Daniel Carver, Chrystian C. Sosa, Colin K. Khoury, and Julian Ramirez-Villegas
Other contributors: Harold A. Achicanoy, Maria Victoria Diaz, Steven Sotelo, Nora P. Castaneda-Alvarez

## References
Ramirez-Villegas J, Khoury CK, Jarvis A, Debouck DG, Guarino L (2010) A gap analysis methodology for collecting crop genepools: a case study with Phaseolus beans. PLoS One 5, e13497. [doi:10.1371/journal.pone.0013497](http://dx.doi.org/10.1371%2Fjournal.pone.0013497)

Khoury CK, Amariles D, Soto JS, Diaz MV, Sotelo S, Sosa CC, Ramírez-Villegas J, Achicanoy HA, Velásquez-Tibatá J, Guarino L, León B, Navarro-Racines C, Castañeda-Álvarez NP, Dempewolf H, Wiersema JH, and Jarvis A (2019) Comprehensiveness of conservation of useful wild plants: an operational indicator for biodiversity and sustainable development targets. Ecological Indicators 98: 420-429. doi: [10.1016/j.ecolind.2018.11.016](https://doi.org/10.1016/j.ecolind.2018.11.016)

Castañeda-Álvarez NP*, Khoury CK*, Achicanoy HA, Bernau V, Dempewolf H, Eastwood RJ, Guarino L, Harker RH, Jarvis A, Maxted N, Mueller JV, Ramírez-Villegas J, Sosa CC, Struik PC, Vincent H, and Toll J (2016) Global conservation priorities for crop wild relatives. Nature Plants 2(4): 16022. doi: [10.1038/nplants.2016.22](http://www.nature.com/articles/nplants201622)

## License
GNU GENERAL PUBLIC LICENSE Version 3
