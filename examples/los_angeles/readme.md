This code will allow you to create: 
(1) A mobility matrix depicting the percent change from baseline in journeys to and from specified areas. 
(2) A city-level situation report with the following plots: 
- Map with outline of city jurisdiction boundary 
- List of top 5 locations in/out of the city based on raw number of trips on the most recent day of data 
- Plots showing changes in trips compared to baseline by location in/out 

To follow along with the code here, please download the Los Angeles and San Diego Movement between Administrative Region FB maps, shape files for [LA cities](https://data.lacounty.gov/dataset/City-Boundaries/eywn-daeu), and shape files for [CA County Census Divisions](https://catalog.data.gov/dataset/tiger-line-shapefile-2016-state-california-current-county-subdivision-state-based) (CCD). 

Trips between the city of interest and other locations are identified as 
(1) between the city of interest and another city in the same larger region (e.g. from city to city in the same CCD) 
(2) between the city of interest and a different larger region (CCD) 
(3) between the city of interest and a location outside of the area of interest (e.g. outside of Los Angeles County) 

Ideally, a mapping file will be available between the smaller and larger administrative regions being used in this analysis. In the code given here, a mapping file was unavailable so cities are assigned to the CCD with which they have the largest overlap.

Instructions: 
1. Download movement files (with geometry) for your area and time frame of interest. 
2. Install and load required packages. 
3. Set run parameters, including paths for downloaded files. 
4. Separate functions are included for the mobility matrix and city-level analyses. 
5. Running the ’Subcounty-Level Analyses’ loop will create mobility matrices for all sub counties and city-level plots for all cities. This code does not save plots. 

Please reach out to Christine at ctedijanto@g.harvard.edu if you have any questions on this code.

