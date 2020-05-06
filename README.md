# GapAnalysis R package

## Description
The GapAnalysis R package provides a series of functions that allows the user to evaluate the in situ and ex situ conservation status of a plant species in a standardized and reproducible way. This method will generate quantitative metrics and spatial outputs which represent the need for conservation and where gap in the collection/conservation of the species exist on the landscape.

The GapAnalysis functions requires the user to provide two inputs
A `data.frame` of species occurrences
A `raster` object of the predicted potential habitat

This library consists of 16 functions. These are split between 4 categories; pre-analysis, in-situ, ex-situ, and combined assessment. The pre-analysis steps establishes the data structure and cleans the data. The in-situ and ex-situ fuctions produce the quantitative conservation assessments.

While each step of the process can be ran as a stand alone function the library is intended to be used following the example workflow provided below.

As with any evaluation the quality and completeness of the data used in the assessment is the limiting factor to the overall usefulness of these metrics. A full description of the application of this process can be found here(link to the paper eventually)

## Table of Contents
? doesn't seems like there is a clean way to automate this in git hub markup... Might have to look into intext reference once structure is finalized. It will be important to have sense this is a rather larger readme

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
We recommend downloading the following example [dataset][link to download]. This is data for four species of the Cucurbita genus. A full gap analysis for this species was performed in 2019 and can be viewed [here](https://nph.onlinelibrary.wiley.com/doi/full/10.1002/ppp3.10085).

The taxon name used as a reference to connect all inputs and outputs throughout the various steps in this process. The name of the taxon must be present for all rows in the csv. This name much also be present in the file name of the raster used to represent the potential suitable habitat of the species.

As you are transforming your own dataset please insure you are using the following parameters.

Coordinates: Decimal Degrees - unprojected WGS1984
Raster: unprojected WGS1984

Information on transforming your data to WGS1984 can be found [here](https://geocompr.robinlovelace.net/reproj-geo-data.html)

### data inputs
CSV dataset of species occurrences and type. This process can handle single and multiple species.

taxon | longitude | latitude | type
------------ | ------------- | -------------| -------------
Cucurbita_cordata | -113.563 | 28.9457 | G
Cucurbita_digitata |  | | H


Taxon: this value will be the key for all functions in this library. Ensure it is consistent for all records and is included in the file name of your predicted potential habitat .tif as well.

Type: The type column refers to if the occurrence data represents a herbarium sample or a germplasm sample. Type H, for herbarium, is a known occurrence of the species in the landscape. Type G, for genebank, is a known occurrence of a species that has been collected and is stored as a living sample either in botanical gardens or genebanks.

This distinction is significant for multiple evaluations and effort must be taken to ensure the correct assignment of these values.

As of 2019, the major sources for Genebank occurrence data that the authors have evaluated include.
[USDA GRIN](https://npgsweb.ars-grin.gov/gringlobal/search.aspx)
[GENESYS](https://www.genesys-pgr.org/)
[WIEWS](need a site for this)
Generally, data from these sources would be considered a “G” type if it is still an active accession. Duplication between these source may exist.

Digital repositories such as GBIF, EDDmaps, and IDIGBIO, are most likely to contain observed locations of a species with no living sample. These would be considered “H” type occurrences. As these databases are not actively curated there is potential for duplicates between sources. Please evaluate this potential when gathering your data and assigning type.

Other independent sources of data need to be evaluated for type on a case by case basis.

More information and examples of how to make the distinction between “H” and “G” points can be found [here](link to either detailed tutorial or paper that defines these distinctions in more detail).


tif of Predicted Potential Habitat

The raster representing the predicted potential extent of suitable habitat is used buy multiple functions to represent the maximum potential range of a species. The development of a high quality representation of a species potential habitat is a field of study in it’s own right. The importance here is that you trust the representativeness of the data you are inputting into this process.

### Workflow
Main_code - example of the recommend workflow.

**preAnalysis**

 - Create_folder_structure
 - Clean_records
 - Create_buffers

**Ex Situ Analysis**
 - SRSex
 - ERSex
 - GRSex
 - FCSin
 - gapMapEx

**In Situ Analysis**
 - SRSin
 - ERSin
 - GRSin
 - FCSex
 - gapMapIn

**summaryEvaluations**   
 - FCSmean
 - EOOandAOO
 - summaryDocument

While each function can be run as a stand alone method. The authors highly recommend following this workflow as it will ensure dependencies for individual functions are in place and that the folders are stored in the correct location to successfully produce the final summary document.

### Example File Structure
workplace
|_ userData*
|  |_ occurenceData
|  |_ rasters
|_gapAnalysis
  |_ species1
  |  |_ version
  |  |  |_ gapAnalysis_outputs
  |  |  |  |_ exsitu
  |  |  |  |_ instu
  |  |  |  |_ combined
  |  |  |_ redlist
  |  |_ summaryDocuments
  |_ species2
     |_ version
     |  |_ gapAnalysis_outputs
     |  |  |_ exsitu
     |  |  |_ instu
     |  |  |_ combined
     |  |_ redlist
     |_ summaryDocuments

The user needs to provide the occurrence data and rasters. All other files will be generated by following the provided workflow.


### preAnalysis step

The pre analysis steps are preform across all unique species at once. The steps are required to generate the data and file structure that will be used in the following gap analysis functions.

**Create_folder_structure**
Relative path names are used in all the in situ and ex situ conservation metrics. This fucntion established those path to ensure consistency through the following processes.

**generateCounts**
This function produces counts of species occurrences based on the type(G or H), presence of quality latitude and longitude values, and combinations of those qualities. The code generate a counts.csv which is used in subsequent gap analysis functions.  

**Create_buffers**
This function buffer all know "G" occurrences and mask those buffers to the area within a the predicted potential suitable habitat provided by the user. The output of this process is a raster layer that is used in multiple gap analysis metrics.

### Ex situ Analysis

The ex situ analysis evaluates the extent to which a species is preserved in the genebanks and botanical gardens of the world. These living samples represent long term storage of the plant outside of the species native habitat. Ex situ conservation is an important process for ensuring the conservation of a species as native habitat is directly effected by landscape and ecological changes. Additionally, many genebanks distribute seeds to the scientific research community, as a result samples that have been conserved ex situ are more likely to be represented in the ongoing research of the species.

For the purpose of this library all occurrences that are representative of living samples stored in genebanks or botanical gardens are denotes as Type:”G” —G stands for germplasm.

**SRSex**
The sampling representativeness score ex situ measure the relationship between the number of germplasm accessions(G) verses the total number of reference records (H). Which would indication that when a species is being observed in is natural landscape, seed is also being collected and preserved in the genebank system. Unlike all other metrics, SRSex takes into account all records, even those that do not have latitude and longitude values.

(total number of G accession)/(total number of H accession) * 100

An ideal SRSex of one would imply that germplasm collection is occurring at the same rate as the location of a given species is being defined.  


**GRSex**
The geographic representativeness score ex situ calculates the proportion of the predicted potential habit that falls will 50 km of a G occurrence over the total area of the predicted potential habitat. The buffer distance implies that an accession of a species is representative of the genetic character of the population of the species within a that buffered area. The assumption maybe be more or less true for different species and the buffer distance can be altered in the main_code.r script. We recommend only changing this value if you have species specific information to do so.

(area within 50km of G points, masked to known potential habitat) / ( total area of predicted potential habitat) * 100

An ideal GRSex score of 100 implies that all of the predicted potential habitat of a species is found within 50 km of a G occurrence.

**ERSex**
The ecological representativeness score ex situ compares the number of distinct ecoregions within the predicted range of a species against the number of ecoregions which have G accessions collected from within them.  

(number of ecoregions from which a G accessions has been collected) / (number of ecoregions present in the potential predicted habitat ) * 100

An ideal ERSex of 100 would imply that all ecoregions present in the species habitat have at least one G occurrence collected from within it.

**FCSex**
Compiles the SRSex,GRSex, and ERSex into a single metric by averaging all three values.

**GapMapEx**
A spatial representation of the GRSex metric created by masking the predicted potential habitat against the buffers of the G occurrences. The resulting maps shows areas that could have the species present on the landscape and have yet to have any germplasm samples collected from the region.

### In situ Conservation Assessment
    The in situ conservation assessment evaluated how well protected a species native habitat is. The World Database of Protected Areas(WDPA)(IUCN,2019) is used to determine areas which are designated, inscribed or established protected areas.

**SRSin**
The sampling representativeness score in situ calculated the proportion of occurrences that fall within protected areas compared to occurences outside of protected areas.

(Number of occurrences inside protected areas) / (Total number of occurrences) * 100

An ideal SRS score of 100 would imply that all known occurrences of a species are within a protected area.

**GRSin**
The geographic representativeness in situ score calculates the proportion of the predicted suitable habitat that falls within protected areas.

(area of predicted habitat within protected areas) / (area of predicted habitat) * 100

An idea GRSin score of 100 would imply the entirety of the species predicted habitat occurs within a protected area.

**ERSin**
The ecological representativeness score in situ calculates the proportion of ecoregions that contain a protected area. Only ecoregions in which a known occurrence is found area included in this assessment.

(number of ecoregions that contain a protected area) / (number of ecoregions in which a known occurrence of the species is found) * 100

An ideal ERSin score of 100 would imply that there is at least 1 protected area in all the ecoregions in which this species is know to exist.

**FCSin**
Compiles the SRSin, GRSin, and ERSin into a single metric by taking the mean of all three values.

**GapMapIn**
A spatial representation of the GRSin metric is created by masking the predicted potential habitat of the species by the WDPA raster. The output shows areas of the potential distribution that are not currently within designated, inscribed or established protected areas.


### Summary Evaluations
**FCS-mean**
Complies the FCSex and FCSin into a single metric by taking the mean of the

**EOOandAOO**
The Extent of Occurrence (EOO) and Area of Occurrence functions from the library redlistR[https://github.com/red-list-ecosystem/redlistr] are include as part of the gap analysis method as the represent an additional standard for accessing the conservation status of a species. These metrics are used as part of the process for defining the International Union for Conservation of Nature Red List of Threatened Species.

Unlike many of the gapAnalysisR functions the EOO and AOO metrics generally more sensitive to the spatial extent represented by the occurrences data. For example; if all the occurrences of a given species are found within a single protected area, the in situ metric gap analysis metrics would be very high, but the EOO and AOO metrics would be low. The balance between these evaluations methods should hopefully provide a more complete quantification of a species conservation status.  

**speciesSummary**  
As long as there are no major deviations from the established file structure the speciesSummary function calls a Rmarkdown file that compiles the results of all above metrics into a single interactive html document that can be shared and opened directly via a web browser. This user friendly format should help your own evaluations and assessments and allow you to share your work with colleagues.

## Authors
Main: Daniel Carver, Chrystian C. Sosa, Colin K. Khoury, and Julian Ramirez-Villegas
Other contributors: Harold A. Achicanoy, Maria Victoria Diaz, Steven Sotelo, Nora P. Castaneda-Alvarez

## References
Ramirez-Villegas J, Khoury CK, Jarvis A, Debouck DG, Guarino L (2010) A gap analysis methodology for collecting crop genepools: a case study with Phaseolus beans. PLoS One 5, e13497. doi:10.1371/journal.pone.0013497[http://dx.doi.org/10.1371%2Fjournal.pone.0013497]

Khoury CK, Amariles D, Soto JS, Diaz MV, Sotelo S, Sosa CC, Ramírez-Villegas J, Achicanoy HA, Velásquez-Tibatá J, Guarino L, León B, Navarro-Racines C, Castañeda-Álvarez NP, Dempewolf H, Wiersema JH, and Jarvis A (2019) Comprehensiveness of conservation of useful wild plants: an operational indicator for biodiversity and sustainable development targets. Ecological Indicators 98: 420-429. doi: 10.1016/j.ecolind.2018.11.016[https://doi.org/10.1016/j.ecolind.2018.11.016]

Castañeda-Álvarez NP*, Khoury CK*, Achicanoy HA, Bernau V, Dempewolf H, Eastwood RJ, Guarino L, Harker RH, Jarvis A, Maxted N, Mueller JV, Ramírez-Villegas J, Sosa CC, Struik PC, Vincent H, and Toll J (2016) Global conservation priorities for crop wild relatives. Nature Plants 2(4): 16022. doi: 10.1038/nplants.2016.22[http://www.nature.com/articles/nplants201622]

## License
GNU GENERAL PUBLIC LICENSE Version 3
