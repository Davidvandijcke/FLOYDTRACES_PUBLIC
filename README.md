# FLOYDTRACES_PUBLIC
A public repository containing code and data from the paper "Using Mobile Device Traces to Improve Near-Real Time Data Collection During the George Floyd Protests"

Get the protests data (here)[]


## CODE

This code is a work in progress and is not meant to run out of the box. It is made available to facilitate further research and provide insight into how the result from the paper and the public data were produced. The scripts below builds on code from (here)[https://colab.research.google.com/drive/1qqLRxehVZr1OBpnbHRRyXPWo1Q98dnxA?usp=sharing] and (this paper)[https://www.pnas.org/content/117/33/19837/tab-figures-data]. We thank David Holtz for sharing the code for the latter and SafeGraph for their support. 

### R 
* *programs.Rproj*: open project in RStudio to start
* *000_master.R*: Sets paths and runs:
  * *00_prep.R*: loads libraries and does housekeeping
  * *01_fts.R*: loads user-written functions
* *10_protestCties.R*: compiles list of protest locations from various sources for comparison
* *12_create_geo_adjacency_matrix.r*: creates a dyadic graph of origin-destination CBGs with mobile device counts by day, using the output from *build-geo-[...].py* below
* *20_analysis.R*: creates figures in the paper, plus unused ones
* *21_publicData.R*: outputs publicly shareable, aggregated data

### Python
* *build-geo-adjacency_cbgProtests.py*: extract graph (destination to origin CBG) info from raw SafeGraph data
* *samplingBias.py*: correct device counts for sampling bias using the Census (NB this is a separate script for purely historical reasons)

## DATA

* *protests[..].csv*: data on protest locations and times from various sources, for comparison
* *tract[..].csv*: tract FIPS to ZIP code mapping, to link CBGs to cities
* *floydDeviceTraces_public.csv.gz*: main data file containing estimates of which CBGs saw George Floyd protests. Structure below:

| dest_cbg                                     | ds             | city        | state        | outside_perc_wkX                                                                                      | Protest                                                                  | protEver                                                                                 | xcoord   | ycoord    | outside_perc_wkX_aboveY                                                                                                                                                                                                                                                                                                                 |
|----------------------------------------------|----------------|-------------|--------------|-------------------------------------------------------------------------------------------------------|--------------------------------------------------------------------------|------------------------------------------------------------------------------------------|----------|-----------|-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| Census Block Group to which devices traveled | Date of travel | City of CBG | State of CBG | Ratio of number of devices that traveled to CBG on given day compared to X-week average before May 26 | Did a protest occur in this city on this day according to other sources? | Did a protest "ever" (in the May 26-June 6 period) occur here according to other sources | Latitude | Longitude | Dummy for whether device ratio surge compared to average X weeks before May 26 was above Y, where Y = 200,300,400,500. For a threshold of e.g. 200, any CBG that sees a number of devices on a given day after May 26 that is more than 2x the average number of devices in the X weeks before May 26 is denoted as a protest location. |
