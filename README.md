# COVID-19 Mobility Data Network: Situation Reports

## Background
We are a network of infectious disease epidemiologists at universities around the world working with technology companies to use aggregated mobility data to support the COVID-19 response. Our goal is to provide daily updates to decision-makers at the state and local levels on how well social distancing interventions are working, using anonymized, aggregated data sets from mobile device, and to provide them analytic support for interpretation. You can read more and connect [here](covid19mobility.org).

## Purpose

There is no *one-format-fits-all* situation report.  

The needs of local partners and even the interpretability of certain mobility metrics will change given the spatial scale of interest and the requested frequency of updates.

In this repo we aim to provide tools for:  

**Accessing Data**  
- [x] Facebook (FB) - currently accessed through the [geoinsights portal](www.facebook.com/geoinsights-portal/).
- [ ] Camber - TBD
- [ ] Cuebiq - TBD

**Mapping Data Locally**
- [x] FB movement data
- [x] FB population data
- [ ] Camber data - TBD
- [ ] Cuebiq data - TBD

**Maintaining a standard code for useful plots**
- [x] FB percent change in movement line graphs
- [x] FB total distance line graphs
- [x] FB percent change in movement maps
- [x] FB local travel network maps
- [ ] Camber plots
- [ ] Cuebiq plots

**Encouraging collaboration**  
As researchers generate analyses for different regions, this repository will act as a collaborative workspace to share pipelines, functions and code snipets for:
- report templates
- plots
- modeling examples

**Communication**  
If you have yet to become part of the project please reach out at through our
main website [here](www.covid19mobility.org). Once onboarded you can request direct
access to this repository and Basecamp which we are using as our project management tool.

**Availability**  
This repository will remain open and available for forking. Members of the collaboration
will be added as contributors to the main code base. Please see below for details on
how to contribute.

## Directory layout

sitrep     
  ├── data  &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; -- *ignored in git*  
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;├── [project_area] &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; -- *specific to your project, `NYC`, `India`, etc...*  
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;├── gis   
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;├── movement  
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;├── pop  
  ├── docs     
  ├── examples&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;  * **this is your working directory**  
  ├── figs    &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; -- *ignored in git*  
  ├── misc    &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; -- *miscellaneous resources*  
  ├── output  
  ├── src                 
  ├── templates               
  ├── LICENSE   
  └── README.md  

## Situation Report Workflow
This is merely a proposed workflow. Researchers are welcome to exit at any step
and conduct their own analyses as long as reproducible and reliable reports
are able to be generated at regular intervals for our local partners.

### Setting up the workspace
All dependencies are tracked in `/src/dependencies.R`. When setting up for the first time please run:

```r
package_list <- c(
  "tidyverse",
  "rmarkdown",
  "sf",
  "lubridate",
  "ggpubr",
  "scales",
  "measurements"
)


#install dependencies
if(T){
  install.packages(package_list)
}

```
After this initial installation,
```r
source("../src/dependencies.R")
```
which is run at the top of every script will ensure that all required libraries
are referenced. If you need to add new libraries it is best to include them in
the package list in `/src/dependencies.R`.

**Local issues with dependencies**  
Unfortunately we are unable to provide support for all possible edge cases
that might arise in regards to the installation and importation of all dependencies.
For example, there is a known issue with some MAC users having difficulty
loading the package `sf` which is required for many mapping tasks. We hope that
this repository and Basecamp will become a location for collaboration over
troubleshooting these issues.

### Workflow

![workflow](/misc/workflow.png)

#### 1) Sourcing the data

#### Facebook
**Accessing FB data**
- Currently the best way to access FB data is through the [geoinsights portal](www.facebook.com/geoinsights-portal/), if you are part of this collaboration but have not been able to access the portal
please reach out through Basecamp.
- [Shenyue Jia](https://www.linkedin.com/in/syjia/) has created a wonderful (unofficial) guide to downloading and working with the [disease prevention maps](https://github.com/syjia/fbcolocation/blob/master/FB_Disease_Prevention_Map_Quick_Start_Guide.pdf) and [collocation data](https://github.com/syjia/fbcolocation/blob/master/Quick_Start_Guide_FB_Colocation_v0315_2020.pdf). If you're unsure how to download or orient yourself with the FB data we recommend that you start there. *Disclaimer:* In certain cases the `Download All` button will not work due to the size of the data. At this point you will need to download the dataset of interest individually per time block using `Download One`.

**Choosing the type of data product for movement**  
If you're interested in looking at vectors of travel between different regions,
it is important to consider the spatial scale of the data available. There are primarily
two different types of mobility data on FB which reference different polygons for aggregation
of the FB population for analysis:

1) **Tile:** These are [Bing Tiles](https://docs.microsoft.com/en-us/bingmaps/articles/bing-maps-tile-system) and provided at various zoom levels. The most spatially granular is at zoom level 16. This
roughly translates to ~0.6km x 0.6km grids near the equator.

2) **Admin Regions:** These are polygons defined by [Pitney Bowes](https://www.pitneybowes.com/us/data/boundary-data/global-boundaries.html) and provided at varying scales ranging from `admin 2`(lowest spatial granularity) to `admin 4`(highest spatial granularity).

Depending on the spatial scale you wish you work at, you may want to use one product vs. another.  

For example:

- If you are interested in analyzing movement data for an entire country, using data from admin regions
may be useful as they will generally "scale" with population density and provide a more
nuanced view in urban areas.

- If you are interested in looking at city level data, tile might be best as they might provide
more spatially granular information at the highest zoom level (0.6km x 0.6km grids) than admin
regions (admin 4 ~ the size of a zip code area in the US).

**Caution:**  
Integration of data from varying spatial scales should be conducted carefully
to avoid double counting of the FB population.

**Additional Resources:**  
In an effort to avoid duplicating efforts we will not describe the full datasets available
here along with all of the variables that they provide. You can find:  

1) Basic information on the FB products [here](https://www.facebook.com/help/geoinsights).  

2) A paper describing more technical aspects [here](https://research.fb.com/publications/facebook-disaster-maps-aggregate-insights-for-crisis-response-recovery/).  

3) The quick start guides referenced above.  

**Organizing the data**
- For this repo we recommend that all data are organized in the `./data/` folder referenced in the directory structure above. This folder is included in the `.gitignore`. Please **do not upload any data** to this repository.
- Currently FB data are read in as `.csv` and require the `geometry` column to be able to appropriately map the [WKT](https://en.wikipedia.org/wiki/Well-known_text_representation_of_geometry) to your local region of interest.

**Tutorial**  

Below we present a brief tutorial on using FB movement data to generate a report for New York City.
To follow along, you will need to have `tile level movement` data from the `New York City Coronavirus`
dataset along with a shapefile for the neighborhood tabulation areas in NYC.

As described above in sourcing the data, the granularity and the spatial map of interest were
decided after coordination with local partners in NYC. 

 Both of these raw
datasets are provided to collaboration members who have already signed the necessary
data licensing / use agreements [here]().

### 2) Convert and map raw data

### 3) Transform dataset

### 4) Generating standard plots

### 5) Provide analysis and interpretation

### 6) Publish situation Reports

## Adding to or editing this repository

**Bugs and fixes**

**Adding templates, plots or other functions**

**Contributing code that doesn't fit in situation reports**

## Upcoming Features
- Building a container for
- Getting and manipulating data from Camber
- Getting and manipulating data from Cuebiq
- Integration with Basecamp API to upload reports
