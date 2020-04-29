### Read in packages -----
library(tidyverse)
library(readxl)
library(sf)
library(ggplot2)
library(ggpubr)
library(raster)
library(lubridate)
library(scales)
library(gganimate)
library(osmdata)
library(ggmap)
library(ggrepel)
library(RColorBrewer)
library(gridExtra)
library(rlist)
library(splitstackshape)
library(measurements)

### Run parameters -----
wd <- "/Users/ctedijanto/Documents/03 Research/11 CA mobility" #working drive

tz <- "US/Pacific" #timezone

fb_data_filename <- paste0(wd, "/01 Data/los angeles - admin - new") #drive with FB mobility data
map_filename <- paste0(wd, "/02 Maps/LA_Cities") #file with maps (MAIN area of interest) to be used
larger_map_filename <- paste0(wd, "/02 Maps/CA_Subcounties_2016") #file with maps (LARGER area of interest) to be used

region_name <- "LCITY" #column name that you would like to keep for MAIN region maps
larger_region_name <- "NAME" #column name that you would like to keep for LARGER region maps

### Load Facebook data -----
## Load data from movement maps
list.files(path =fb_data_filename, pattern = "*?\\.csv$", full.names = T) %>%
  lapply(read_csv) %>%
  bind_rows() %>%
  mutate("Date Time"=with_tz(date_time, tz=tz)) %>% #adjusts dates to given time zone
  mutate(time = as.integer(format(`Date Time`, '%H'))) %>% #separate date and hour
  mutate(date = as.Date(date_time, tz=tz)) -> daytime_out 

first_date <- head(daytime_out$date)[1]
last_date <- tail(daytime_out$date)[1]

### Load maps -----
#load map
list.files(path = map_filename, pattern = "*?\\.shp$", full.names = T) %>%
  lapply(st_read) %>%
  purrr::reduce(st_combine) %>% 
  st_transform(crs = 4326) %>% 
  dplyr::rename("region_name"=region_name) %>% #rename column of interest to 'region_name'
  group_by(region_name) %>% #this line & next 2 combine geometries with the same region_name (do not need if map file does not have mult geometries with same name)
  summarise(geometry = sf::st_union(geometry)) %>% 
  ungroup() -> map

#load map with larger regions
list.files(path = larger_map_filename, pattern = "*?\\.shp$", full.names = T) %>%
  lapply(st_read) %>%
  purrr::reduce(st_combine) %>% 
  st_transform(crs = 4326) %>% 
  dplyr::rename("larger_region_name"=larger_region_name) -> larger_region_map

# create mapping between region and larger area (based on largest intersection)
region_to_larger <- st_join(map, larger_region_map, join=st_intersection, left=TRUE, largest=TRUE) %>% 
  dplyr::select(region_name, larger_region_name) %>%
  st_drop_geometry()

### Map vectors to polygons (from shape file) -----
daytime_out %>%
  dplyr::select(-c(start_lon, start_lat, end_lon, end_lat)) %>% #Remove pre-parsed values
  #extract start and end lat/longs from the geometry data
  mutate(geom = str_replace_all(geometry, "[:alpha:]", ""),
         geom = str_replace_all(geom, " \\(", ""),
         geom = str_replace_all(geom, "\\)", ""), 
         geom = str_replace_all(geom, ",", "")) %>%
  separate(geom, c("start_lon", "start_lat", "end_lon", "end_lat"), sep = " ") %>% 
  mutate(start_lon = as.numeric(start_lon), start_lat = as.numeric(start_lat)) %>% 
  #map start points to polygons in map area
  st_as_sf(coords = c("start_lon", "start_lat"), crs = st_crs(map), remove=FALSE) %>% st_join(map) %>% #transform start long and lat to coordinate system of region map
  st_as_sf(coords = c("start_lon", "start_lat"), crs = st_crs(larger_region_map), remove=FALSE) %>% st_join(larger_region_map) %>%
  as_tibble() %>%
  #subset to variables of interest
  dplyr::select(date, n_baseline, n_crisis, start_polygon_name, end_polygon_name, region_name, larger_region_name, length_km, end_lon, end_lat) %>%
  set_names(c("date", "baseline", "crisis", "FB_start_region", "FB_end_region", "region", "start_larger_region", "length", "end_lon", "end_lat")) %>%
  mutate(end_lon = as.numeric(end_lon), end_lat = as.numeric(end_lat)) %>%
  #map end points to counties in map area
  st_as_sf(coords = c("end_lon", "end_lat"), crs = st_crs(map), remove=FALSE) %>% st_join(map) %>% #transform start long and lat to coordinate system of region map
  st_as_sf(coords = c("end_lon", "end_lat"), crs = st_crs(larger_region_map), remove=FALSE) %>% st_join(larger_region_map) %>%
  as_tibble() %>%
  dplyr::select(date, baseline, crisis, length, FB_start_region, FB_end_region, region, region_name, start_larger_region, larger_region_name) %>%
  set_names(c("date", "baseline", "crisis", "length", "FB_start_region", "FB_end_region", "region", "end_region", "start_larger_region", "end_larger_region")) %>%
  mutate(length = conv_unit(length, "km", "mi")) %>% # Change length units to miles
  mutate_at(c("region", "end_region", "start_larger_region", "end_larger_region"), as.character) %>% # Change region names to text to facilitate matching
  mutate(region=replace_na(region, "Out of bounds"),
         end_region=replace_na(end_region, "Out of bounds"),
         start_larger_region=replace_na(start_larger_region, "Out of bounds"),
         end_larger_region=replace_na(end_larger_region, "Out of bounds")) %>% 
  mutate(travel_type=case_when( # Categorize trips as 'within' or 'between' regions
    region=="Out of bounds" & end_region=="Out of bounds"~"Out of bounds",
    region==end_region~"Within regions",
    TRUE~"Between regions")) %>% 
  mutate(length=case_when( #Update 0 lengths to be very short!
    length==0~0.001,
    TRUE~length)) -> tmp

### Create functions: SUMMARY STATISTICS from data -----
#group data by date and region of interest (in this case polygons)
#calculate percent different (by the START region)
summarize_trips <- function(df_grouped){
  
  df_grouped %>% 
    summarise(baseline = sum(baseline), 
              crisis = sum(crisis)) %>%
    mutate(perc_change = (crisis-baseline)/baseline) -> daytime_out_sum    
  
  return(daytime_out_sum)  
}

### Create function: MOBILITY MATRIX -----
create_mobility_matrix <- function(df){
  
  df_curr <- df %>% filter(date==last_date)
  unique_regions <- unique(append(c(df_curr$region), c(df_curr$end_region))) #pull list of all regions
  region_levels <- append(sort(unique_regions[which(unique_regions!="Out of bounds")]), "Out of subcounty") #order so that "Out of subcounty" is last in matrix
  
  expand.grid(unique_regions, unique_regions) %>% 
    rename("region"="Var1", "end_region"="Var2") %>% 
    left_join(df_curr %>% filter(date==last_date), by=c("region", "end_region")) %>% 
    filter(region!="Out of bounds") %>% #do not show trips that start "Out of bounds"
    mutate(end_region=case_when(end_region=="Out of bounds"~"Out of subcounty", TRUE~end_region)) %>% #rename "Out of bounds" as "Out of subcounty"
    ggplot(aes(x=end_region, y=region, fill=perc_change, label=perc_change)) +
    geom_tile(color="black") +
    scale_y_discrete(limits=rev(sort(unique_regions[which(unique_regions!="Out of bounds")])), expand=c(0,0)) +
    scale_x_discrete(position="top", limits=region_levels, expand=c(0,0)) +
    scale_fill_gradient2(
      low = muted("blue"),
      mid = "white",
      high = muted("red"),
      midpoint = 0,
      space = "Lab",
      na.value = "white",
      guide = "colourbar",
      aesthetics = "fill",
      limits = c(-1,1),
      label = scales::percent) +
    geom_text(aes(label=scales::percent(round(perc_change,2)))) + #show text as % (no decimals)
    labs(title="Mobility matrix", 
         x="Ending city",
         y="Starting city") +
    theme_bw() +
    theme(axis.text.y=element_text(size=10),
          axis.text.x=element_text(size=8), 
          title=element_text(size=14, face="bold"),
          axis.title=element_text(size=12, face="plain"),
          plot.margin=unit(c(0,0.5,0,0),"cm"),
          legend.position="none") -> fig
  
  return(fig)
}

### Create function: CITY-LEVEL ANALYSES -----
city_analysis <- function(curr_city, map_s, tmp_s_merged){
  
  # create map with curr_city outlined
  bbox <- st_bbox(map_s)
  names(bbox) <- c("left", "bottom", "right", "top")
  
  ggmap::get_map(bbox) %>%
    ggmap() +
    coord_sf(crs=st_crs(4326)) +
    geom_sf(data=map %>% filter(region_name==curr_city),
            color = "black", alpha=0.8, size=0.5, inherit.aes=FALSE) +
    theme(axis.title=element_blank(), axis.text=element_blank(), axis.ticks=element_blank()) -> map_city
  
  # summarize trips for CCD
  daytime_out_merged <- tmp_s_merged %>%
    group_by(date, start_merged_region, end_merged_region) %>%
    summarize_trips()

  # skip city if there are 0 trips starting or ending in the city
  if(nrow(tmp_s_merged %>% filter(start_merged_region==curr_city))==0 |
     nrow(tmp_s_merged %>% filter(end_merged_region==curr_city))==0){
    print(paste("Not enough data to create sitrep for", curr_city))
    
  } else {
  
  # skip location in/out tables if no trips in/out on most recent date
  if(nrow(daytime_out_merged %>% filter(start_merged_region==curr_city, date==last_date))>0 &
     nrow(daytime_out_merged %>% filter(end_merged_region==curr_city, date==last_date))>0){  
    
    daytime_out_merged %>%
      filter(date==last_date, start_merged_region==curr_city) %>% 
      arrange(desc(crisis)) %>%
      ungroup() %>% 
      dplyr::select(end_merged_region) %>% 
      head(5) %>% 
      ggtexttable(theme=ttheme("minimal"), cols=c(paste0("Top 5 destinations of trips \nFROM ",curr_city," on ", format(last_date, "%b %d")))) -> locations_out
    
    daytime_out_merged %>%
      filter(date==last_date, end_merged_region==curr_city) %>% 
      arrange(desc(crisis)) %>%
      ungroup() %>% 
      dplyr::select(start_merged_region) %>% 
      head(5) %>% 
      ggtexttable(theme=ttheme("minimal"), cols=c(paste0("Top 5 origins of trips \nTO ",curr_city, " on ", format(last_date, "%b %d")))) -> locations_in
    
    ggarrange(map_city, locations_out, locations_in,
              nrow=1, align="h", widths=c(1,1.3,1.3)) -> top_row
    
  } else {
    map_city -> top_row
    print(paste("Not enough data to create locations in/out for", curr_city))
  }
  
  # Create list of merged regions (including cities in same CCD, other CCDs with journeys to/from)
  list_of_merged_regions <- unique(append(tmp_s_merged$start_merged_region[which(tmp_s_merged$end_merged_region==curr_city)],
                                          tmp_s_merged$end_merged_region[which(tmp_s_merged$start_merged_region==curr_city)]))
  
  # Graph for trips STARTING in region
  daytime_out_merged %>%
    filter(start_merged_region==curr_city) %>% 
    right_join(expand.grid(date=seq(from=first_date, to=last_date, by=1), #right_join to data frame with ALL options (allows single dots to appear in graph)
                           start_merged_region=curr_city,
                           end_merged_region=list_of_merged_regions),
               by=c("date", "start_merged_region", "end_merged_region")) %>%
    mutate(weekend = case_when((wday(date)==1 | wday(date)==7)~"Weekend", TRUE~"Weekday")) %>% #flag weekends to shade backgrounds grey over weekend
    filter(end_merged_region!=curr_city) %>% 
    mutate(end_region_type=case_when(
      grepl("*County", end_merged_region)~"Outside of LA county",
      grepl("*CCD", end_merged_region)~"Outside of CCD",
      TRUE~"Inside of CCD")) %>%
    ggplot(aes(x=date, y=perc_change, color=end_merged_region, linetype=end_region_type)) +
    geom_rect(data=(. %>% filter(weekend=="Weekend")),
              aes(xmin=date-0.5, xmax=date+0.5), ymin=-Inf, ymax=Inf,
              fill="grey85", alpha=0.5, color="grey85") +
    geom_point() + geom_line() +
    scale_linetype_manual(values=c("Outside of LA county"="dotted", "Outside of CCD"="dashed", "Inside of CCD"="solid"),
                          guide="none") +
    scale_y_continuous(labels=scales::percent, limits=c(-1, 0.5)) +
    geom_vline(xintercept=as.Date("2020-03-15"), color="black", size=1.2, linetype="dotted") + #draw dotted line to represent start of Shelter-In-Place
    labs(x="Date", y="Percent change",
         title="Percent change in trips STARTING in region compared to baseline",
         color="", linetype="") +
    theme_bw() +
    theme(axis.text=element_text(size=10),
          axis.title=element_text(size=12, face="plain"),
          title=element_text(size=14, face="bold"),
          legend.text=element_text(size=10),
          legend.key=element_rect(color="white", fill="white"),
          legend.position="none",
          plot.margin=unit(c(0,0.5,0,0), "cm")) -> dest_fig
  
  # Graph for trips ENDING in region
  daytime_out_merged %>%
    filter(end_merged_region==curr_city) %>% 
    right_join(expand.grid(date=seq(from=first_date, to=last_date, by=1), #right_join to data frame with ALL options (allows single dots to appear in graph)
                           end_merged_region=curr_city,
                           start_merged_region=list_of_merged_regions),
               by=c("date", "start_merged_region", "end_merged_region")) %>%
    filter(start_merged_region!=curr_city) %>% 
    mutate(weekend = case_when((wday(date)==1 | wday(date)==7)~"Weekend", TRUE~"Weekday")) %>% #flag weekends to shade backgrounds grey over weekend
    mutate(start_region_type=case_when(
      grepl("*County", start_merged_region)~"Outside of LA county",
      grepl("*CCD", start_merged_region)~"Outside of CCD",
      TRUE~"Inside of CCD")) %>%
    ggplot(aes(x=date, y=perc_change, color=start_merged_region, linetype=start_region_type)) +
    geom_rect(data=(. %>% filter(weekend=="Weekend")),
              aes(xmin=date-0.5, xmax=date+0.5), ymin=-Inf, ymax=Inf,
              fill="grey85", alpha=0.5, color="grey85") +
    geom_point() + geom_line() +
    scale_linetype_manual(values=c("Outside of LA county"="dotted", "Outside of CCD"="dashed", "Inside of CCD"="solid")) +
    scale_y_continuous(labels=scales::percent, limits=c(-1, 0.5)) +
    geom_vline(xintercept=as.Date("2020-03-15"), color="black", size=1.2, linetype="dotted") + #draw dotted line to represent start of Shelter-In-Place
    labs(x="Date", y="Percent change",
         title="Percent change in trips ENDING in region compared to baseline",
         color="", linetype="") +
    theme_bw() +
    theme(axis.text=element_text(size=10),
          axis.title=element_text(size=12, face="plain"),
          title=element_text(size=14, face="bold"),
          legend.text=element_text(size=12),
          legend.key=element_rect(color="white", fill="white"),
          legend.position="bottom",
          legend.direction="horizontal",
          legend.box="vertical",
          plot.margin=unit(c(0,0.5,0,0), "cm")) +
    guides(color=guide_legend(ncol=3)) -> origin_fig
  
  ggarrange(dest_fig, origin_fig, align="v", ncol=1, heights=c(1,1.4)) -> bottom_figs
  
  ggarrange(top_row, bottom_figs, ncol=1, align="v", heights=c(0.8,2)) -> city_plot
  
  #print city_plot
  return(city_plot)
  
  }
}

### Run: SUBCOUNTY-LEVEL ANALYSES -----
# subcounty analogous to CCD in the LA County analysis
list_of_subcounties <- unique(region_to_larger$larger_region_name)

# Map
for(s in list_of_subcounties){
  
  # pull list of cities in subcounty (CCD) s
  list_of_cities <- region_to_larger %>%
    filter(larger_region_name==s) %>%
    dplyr::select(region_name) %>% 
    pull()
  
  map_s <- map %>% filter(region_name %in% list_of_cities)
  
  # skip if 0 trips starting in CCD
  if(nrow(tmp %>% filter(region %in% list_of_cities))==0){next}
  
  # summarize % change in trips for cities in subcounty
  tmp %>%
    filter(region %in% list_of_cities) %>% 
    mutate(end_region=case_when(
      (end_region=="Out of bounds" | !(end_region %in% list_of_cities))~"Out of bounds", #reassign 'end_region' so that any end city not in CCD is "Out of bounds"
      TRUE~end_region)) %>% 
    group_by(date, travel_type, region, end_region) %>%
    summarize_trips() %>% 
    create_mobility_matrix() -> matrix
  
  # print matrix
  print(matrix)
  
  # prep data for city-level analyses
  # filter to only trips that started or ended in any city in the CCD
  tmp_s <- tmp %>% filter(region %in% list_of_cities | end_region %in% list_of_cities)
  
  # create 'merged' regions that are city in CCD, other CCD, or outside of LA county
  tmp_s %>% 
    mutate(start_merged_region=case_when(
      region=="Out of bounds"~"Out of LA County",
      !(region %in% list_of_cities)~paste(start_larger_region, "CCD"),
      TRUE~region)) %>% 
    mutate(end_merged_region=case_when(
      end_region=="Out of bounds"~"Out of LA County",
      !(end_region %in% list_of_cities)~paste(end_larger_region, "CCD"),
      TRUE~end_region)) -> tmp_s_merged
  
  if(length(list_of_cities)==0){next}
  for(c in list_of_cities){
    city_plot <- city_analysis(c, map_s, tmp_s_merged)
    print(city_plot)
  }
}
