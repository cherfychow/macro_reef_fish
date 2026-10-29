# MacroReefFish
### A standardised database on global reef fish assembly structure

This repository contains the code used to standardise reef fish survey data, based on the structure used by the [BioTIME database](https://biotime.st-andrews.ac.uk). The data sourced for this database originates from Data Dryad, OBIS, GBIF, NOAA, Pangaea, Github, LTER EDI Portal, BioTIME, and various individual researchers and NGOs. A release including curated data and associated metadata is pending final approval from all data owners.

Disclaimer: This repository may still undergo revisions and changes. 

**Corresponding author**: Cher Chow, cherchow (at) proton.me

#### *In review*

## Data standardisation

Data standardisation fits all data to a common long-format data structure and hierarchy described below. In addition to formatting, we implement quality controls and checks for dates, coordinates, and taxonomy. We ensured that each resulting dataset is associated with a consistent methodology applied across sites and over time. Some studies get split if methods get changed during a monitoring programme or multiple methods are used. The creation of unique sampling event identifiers directly corresponds with the reported methods of each dataset.

Taxonomic records are allowed at any resolution, from Order to species. Some datasets use working names for uncertain or unresolved species, and those are always kept for the purpose of counting distinct taxa. These can only be used nested within a dataset identifier (i.e. unique only as DataID_genus_species). For all records, we match reported taxa with validated names from both WoRMS and FishBase where possible.

### Data hierarchy

- Dataset (`DataID`)
- Sampling event (`SamplingEventID`): a survey unit occurring at a site at a given time. For some datasets, it may consist of multiple replicates (e.g. a set of 5 transects at different depths), but it **must** not vary across space or time. 
- Data record (row): A record of a species with a given abundance and length.
