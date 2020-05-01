# COVID-19 Mobility Data Network: Situation Reports

## Background
We are a network of infectious disease epidemiologists at universities around the world working with technology companies to use aggregated mobility data to support the COVID-19 response. Our goal is to provide daily updates to decision-makers at the state and local levels on how well social distancing interventions are working, using anonymized, aggregated data sets from mobile device, and to provide them analytic support for interpretation. You can read more and connect [here](http://www.covid19mobility.org).

## Examples
| Region | Data Used | Description of output | Language |  
|------| ---------|--------|----------|
|[New York, NY, USA](https://github.com/COVID-19-Mobility-Data-Network/sitrep/tree/master/examples)|Facebook Mobility | A report on comparative change in mobility by neighborhood tabulation areas. | R |
|[Syracuse, NY, USA](https://github.com/COVID-19-Mobility-Data-Network/sitrep/tree/master/examples)|Facebook Population | A report on comparative change in population by census tract. | R |
|[Los Angeles, CA, USA](https://github.com/COVID-19-Mobility-Data-Network/sitrep/tree/master/examples/los_angeles)| Facebook Mobility | A report on comparative change in mobility across cities in LA county. | R |
|[Colorado and Utah, USA](https://github.com/ryanlayer/COvid19) | Facebook Population | We are working with cities and counties in Colorado and Utah to monitor stay-at-home orders using Facebook population density data. We infer activity levels by comparing relative density changes between weeks, between weekdays and weekend ends, and with time-series decomposing to track growth trends independently of regular day/night and weekday/weekend patterns.| Python |


## Purpose

There is no *one-format-fits-all* situation report.  

The needs of local partners and even the interpretability of certain mobility metrics will change given the spatial scale of interest and the requested frequency of updates.

In this repo we aim to provide tools for:  

**Accessing Data**  
- [x] Facebook (FB) - currently accessed through the [geoinsights portal](http://www.facebook.com/geoinsights-portal/).
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
main website [here](http://www.covid19mobility.org). Once onboarded you can request direct
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
- Currently the best way to access FB data is through the [geoinsights portal](http://www.facebook.com/geoinsights-portal/), if you are part of this collaboration but have not been able to access the portal
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

1) Basic information on the FB products [here](https://www.facebook.com/help/geoinsights). (You need to have access to Geoinsights to be able to view this.)

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
data licensing / use agreements [here](https://filecloud.covid19mobility.org/url/mobilitydemo).

The steps below are found in `src/example_nyc_pipeline.R`.

### 2) Convert and map raw data
**Load resources**
```r
source("../src/dependencies.R")
source("../src/ingest_data.R")
source("../src/standard_plots.R")
```
**Map spatial data**  
You can find descriptions of all parameters in `../src/ingest_data.R`.
```r
#map all movement data from FB to
map_fb_movement(path_to_map = "../data/nyc/gis/nyc_tab_areas/geo_export_40a90669-c81b-4107-a2b4-d8361a3bd512.shp",
                path_to_fb_mvmt_data = "../data/nyc/movement_tile/",
                tz = "US/Eastern",
                map_region_name = "ntaname",
                map_crs = 4326,
                read_from_cache = T,
                save_to_cache = T,
                project_area = "NYC",
                project_name = "tile_movement",
                data_type = 1) -> mvmt_output
```
This returns a list as `mvmt_output` where the first item is a cleaned data set with start and stopping points maps to polygons in the shapefile referenced in `path_to_map`. **Important:** The value for `map_region_name` must be unique across all polygons in the map so that you can merge data back into the spatial file.

**What's happening under the hood?**  
- loads FB movement data
- loads spatial data (with unique polygon names as `map_region_name`)
- extracts start and stop lat/lon from WKT of FB movement data
- maps start and stop lat/lon to spatial polygons and extracts the unique `map_region_name`.
- returns start region, stop region, baseline FB people moving, crisis FB people moving and date/time as first item in list
- returns map used for the spatial mapping as second item in list

### 3) Transform dataset [RESEARCHER DRIVEN]
These next steps will be driven by the end analysis that the researcher wants to
produce for the local partners. While this is provided as an example it is by no means the only analysis that can be done.

Use the first item in the returned list. Decide if you want to drop out any of the 8 hours blocks of Facebook time. All time's provided are the start point in the timezone specified in the preceding function. Some 8 hour blocks may, therefore, cross the date boundary. In some cases this may be minimal, however you should decide whether you want to keep this block in your analysis.

For NYC the date time block crosses from 2000-0400. We consider these to be "sleeping hours" and categorize them at beloning to the date that 2000 starts in.
```r
mvmt_output[[1]] %>%
  #extract the hour of the date_time variable
  mutate(time = as.integer(format(date_time, '%H'))) %>%
  #decide if you want to drop any time windows
  #subset(time > 3 & time < 18) %>%
  #convert date_time to date
  mutate(date = as_date(date_time)) %>%
```
We choose to convert the standard measurement of distance `KM` into `Miles` for ease of interpretation in the US.
```r
  #convert km measurements into mi if needed
  mutate(length = conv_unit(length, "km", "mi")) %>%
```
We categorize travel vectors as ending "within" or outside "between" our regions of interest, the neighborhood tabulation areas and create a `dist` variable which calculates the total distance traveled as the number of individuals in `crisis` who moved over the length of that vector.
```r
  #create flag for travel inside and outside regions
  #calculate dist which is total distance traveled
  mutate(flag = ifelse(start_region == end_region, "Within NTA", "Between NTA"),
         dist = length*crisis) %>%
```
We group all our data by `date`, `start_region` (the NTA which corresponds to the starting point of the vector) and `flag`(the value of within or between NTA travel). Finally we sum baseline, crisis and distance measures.
```r
  #summarise values by date, start_region and flag
  group_by(date, start_region, flag) %>%
  summarise(baseline = sum(baseline),
            crisis = sum(crisis),
            dist = sum(dist)) %>%
```
We calculate percent change per aggregated row and output into a dataset for the generation of plots.
```r
  #calculate aggregated percent change
  mutate(perc_change = (crisis-baseline)/baseline) -> nyc_nta
```
### 4) Generating standard plots
Here we provide templates for plots used to generate area wide and NTA specific analyses.   

For our area wide analysis we use the base map along with the data munged above. Please see function in `src/standard_plots.R` for further information about parameters and inputs.
```r
generate_area_plots(data = nyc_nta,
                    map = mvmt_output[[2]],
                    map_region_name = "ntaname",
                    project_area = "NYC",
                    area_of_analysis = "city")
```
We finally split the `nyc_nta` data by NTA and generate a travel network plot for each area.
```r
split(nyc_nta, nyc_nta$start_region) %>%
  lapply(function(x) generate_travel_plots(data = x,
                                           vector_data = mvmt_output[[1]],
                                           map = mvmt_output[[2]],
                                           map_region_name = "ntaname",
                                           project_area = "NYC",
                                           area_of_analysis = "nta",
                                           map_nested_under_name = "boro_name"))
```
The two functions above will create directory files for the `project_area` and the `area_of_analysis`. They will then dump the generated plots in those files. For the generation of the travel network the plots are output with the following naming convention:  

[`total size of travel network`]-[`boro_name`]-[`nta_name`].png  

This naming convention allows for information about the plot to be transmitted to the standard situation reports.

### 5) Provide analysis and interpretation [RESEARCHER DRIVEN]
**This key step** includes the researcher reviewing all the plots presented and evaluating the data in general including information from other data sources to provide updates about key points to the local partner. These key points and updated information should be part of the communication to the local partner in the situation report.

### 6) Publish situation Reports

Here we provide an example of pulling generated plots into a final report that can be shared with your local partner. These can be called with specific date parameters.

```r
rmarkdown::render("../templates/nyc_sitrep.Rmd", params = list(
  date = tail(nyc_nta$date,1)),
  output_format = "html_document",
  output_file = paste0("../output/nyc_sitrep/nyc_sitrep_",tail(nyc_nta$date,1),".html"))

```

The workflow ends with these steps:  
1) **Share** the situation report with the local partner  
2) **Upload** the latest report to Basecamp  
3) **Provide** any feedback or comments about the process in Basecamp  
4) **Raise** any issues with the code in GitHub  

## Adding to or editing this repository

**Bugs and fixes**  
- Raise an issue
- Create a branch with the issue number as the first part of the name of the branch: [`issue number`]-[`descriptive name of bug`]
- Provide the fix and describe how you fixed it
- Generate a pull request and code review
- Your fix will be reviewed and added into the master branch

**Adding templates, plots or other functions**
- Raise an issue
- Create a branch with the issue number as the first part of the name of the branch: [`issue number`]-[`descriptive name of product`]
- Make sure you're able to provide a tutorial in a `.md` and add it to `docs/`
- Modify the primary `README` and add a link to your tutorial
- Make sure you point to data needed to reproduce your product
- Generate a pull request and code review
- Your addition will be reviewed and added into the master branch

**Contributing code that doesn't fit in situation reports**
- There are many more interesting and useful things that can be done with these data such as modeling of disease dynamics or evaluating metapopulation mobility. Please reach out to the administrative team and we will generate a new repository under the same organization to allow for easier collaboration for these research topics.  

## Upcoming Features
- Tutorials for individual functions
- Tutorial for FB pop data
- Building a container for running data ingestion
- Getting and manipulating data from Camber
- Getting and manipulating data from Cuebiq
- Integration with Basecamp API to upload reports

## Technical information
- Session info ------------------------------------  
 setting  value                       
 version  R version 3.6.3 (2020-02-29)
 os       Windows 10 x64              
 system   x86_64, mingw32             
 ui       RStudio                     
 language (EN)                        
 collate  English_United States.1252  
 ctype    English_United States.1252  
 tz       America/New_York            
 date     2020-04-02                  

- Packages ----------------------------------------  
 package      * version date       lib source        
 dplyr        * 0.8.5   2020-03-07 [1] CRAN (R 3.6.3)  
 forcats      * 0.5.0   2020-03-01 [1] CRAN (R 3.6.3)  
 ggplot2      * 3.3.0   2020-03-05 [1] CRAN (R 3.6.3)  
 ggpubr       * 0.2.5   2020-02-13 [1] CRAN (R 3.6.3)   
 lubridate    * 1.7.4   2018-04-11 [1] CRAN (R 3.6.2)  
 magrittr     * 1.5     2014-11-22 [1] CRAN (R 3.6.2)  
 measurements * 1.4.0   2019-05-28 [1] CRAN (R 3.6.0)  
 purrr        * 0.3.3   2019-10-18 [1] CRAN (R 3.6.2)  
 readr        * 1.3.1   2018-12-21 [1] CRAN (R 3.6.2)  
 rmarkdown    * 2.1     2020-01-20 [1] CRAN (R 3.6.3)  
 scales       * 1.1.0   2019-11-18 [1] CRAN (R 3.6.3)  
 sf           * 0.9-0   2020-03-24 [1] CRAN (R 3.6.3)  
 stringr      * 1.4.0   2019-02-10 [1] CRAN (R 3.6.2)  
 tibble       * 3.0.0   2020-03-30 [1] CRAN (R 3.6.3)  
 tidyr        * 1.0.2   2020-01-24 [1] CRAN (R 3.6.3)  
 tidyverse    * 1.3.0   2019-11-21 [1] CRAN (R 3.6.3)  

[1] C:/Users/Nishant/Documents/R/win-library/3.6  
[2] C:/Program Files/R/R-3.6.3/library
