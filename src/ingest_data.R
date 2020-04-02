source("./dependencies.R")

#set of functions and helper functions to read and map FB data


#reads in raw fb movement data and maps it to your spatial scale of interest
#VARS
#' @param path_to_map (string): relative or absolute path to map of interest
#' @param path_to_fb_mvmt_data (string): relative or absolute path to folder containing FB movement data
#' @param tz (string): describe the timezone for the data run Olson() for a list of time zones
#' @param map_region_name (string): the unique variable name in "map" that you want to capture in your vector data
#' @param map_crs (integer): The EPSG number for the crs of interest 
#' @param read_from_cache (boolean): use local cache of previously processed data or run from scratch
#' @param save_to_cache (boolean): save output to local cache for subsequent use
#' @param project_area (string): describe the region that this project refers to 
#' @param projectname (string): the specific type of dataset you're working with for saving the cache
#' @param data_type (numeric): 1 for original data type downloaded sequentially from geoinsights '
#                                 where baseline is represented as "Baseline: People Moving" and 
#                              2 for data downloaded by id where baseline is represented at "n_baseline" 
#                        and stop points for the vectors are weighted by pop contributing info 
#                        or by the centroid of the polygon

#temporary inputs left in for quick bug fixes
# path_to_map <- "../data/nyc/gis/nyc_tab_areas/geo_export_40a90669-c81b-4107-a2b4-d8361a3bd512.shp"
# path_to_fb_mvmt_data <- "../data/nyc/movement_tile/"
# tz <- "US/Eastern"
# map_region_name <- "ntaname"
# map_crs <- 4326
# read_from_cache <- T
# save_to_cache <- T
# project_area <- "NYC"
# project_name <- "tile_movement"
# data_type <- 1


#spatially map vectors of raw FB movement data
map_fb_movement <- function(
  path_to_map, 
  path_to_fb_mvmt_data, 
  tz,
  map_region_name,
  map_crs = 4326,
  read_from_cache = F, 
  save_to_cache = T,
  project_area,
  project_name,
  data_type
){

  #error checking for input parameters
  if(data_type != 1 & data_type != 2){
    stop("Please choose 1 or 2 for the data type")
  }
  if(read_from_cache & !file.exists(paste0("../cache/",project_area,"/",project_name,".rds"))){
    print(paste0("We were unable to find a cached file in: ", paste0("../cache/",project_area,"/",project_name,".rds")))
    print("Running function without reading cached data")
    read_from_cache <- F
  }
  if(!dir.exists(paste0("../cache/", project_area))){
    dir.create(paste0("../cache/", project_area), recursive = T, showWarnings = F)
  }
  if(!file.exists(path_to_map)){
    stop("Map file not found")
  }
  if(!dir.exists(path_to_fb_mvmt_data)){
    stop("Path to FB movement data doesn't exist")
  }
  if(list.files(path_to_fb_mvmt_data, pattern = "*?\\.csv$") %>% length == 0){
    stop("No csv files found in location of movement data")
  }
  
  #load map
  st_read(path_to_map) %>% 
    st_transform(crs = map_crs) -> map #projects to crs of interest
  
  #check to see that the map name variable is unique
  if(pull(map, eval(map_region_name)) %>% unique %>% length != nrow(map)){
    stop("Please ensure that the variable name you have chosen on your map is unique for every polygon")
  }
  
  #load data and convert to local time depending on data type
  if(data_type == 1){
    
    suppressMessages(
      list.files(path = path_to_fb_mvmt_data, pattern = "*?\\.csv$", full.names = T) %>%
        lapply(function(x) read_csv(x, 
                                    col_types = cols_only(
                                      `Date Time` = "T",
                                      `Baseline: People Moving` = "d",
                                      `Crisis: People Moving` = "i",
                                      `Length(km)` = "d",
                                      `Geometry` = "c"
                                    ))) %>%
        bind_rows() %>%
        #adjusts datetime
        mutate("Date Time"=with_tz(`Date Time`, tz=tz)) %>%
        set_names(c("date_time", "length", "baseline", "crisis", "geometry")) -> raw_data 
    )
    
  }else{
    
    suppressMessages(
      list.files(path = path_to_fb_mvmt_data, pattern = "*?\\.csv$", full.names = T) %>%
        lapply(function(x) read_csv(x, 
                                    col_types = cols_only(
                                      `date_time` = "T",
                                      `n_baseline` = "d",
                                      `n_crisis` = "i",
                                      `length_km` = "d",
                                      `geometry` = "c"
                                    ))) %>%
        bind_rows() %>%
        #adjusts datetime
        mutate("date_time"=with_tz(date_time, tz=tz)) %>%
        set_names(c("date_time", "baseline", "crisis", "length", "geometry")) -> raw_data 
    )
    
  }

  
  if(read_from_cache){
    data <- read_rds(paste0("../cache/",project_area,"/",project_name,".rds"))
    raw_data <- subset(raw_data, !(date_time %in% unique(data$date_time)))
  }
  
  
  if(nrow(raw_data) > 0){
    #mung data
    raw_data %>%
      #begin mapping vectors to polygons
      #extract start and end lat/longs from the geometry data
      mutate(geom = str_replace_all(geometry, "[:alpha:]", ""),
             geom = str_replace_all(geom, " \\(", ""),
             geom = str_replace_all(geom, "\\)", ""),
             geom = str_replace_all(geom, ",", "")) %>%
      separate(geom, c("start_lon", "start_lat", "end_lon", "end_lat"), sep = " ") %>%
      mutate(start_lon = as.numeric(start_lon),
             start_lat = as.numeric(start_lat)) %>%
      #map start points to polygons in map area
      st_as_sf(coords = c("start_lon", "start_lat"), crs = st_crs(map)) %>%
      st_join(map) %>%
      as_tibble() %>%
      #subset to variables of interest
      dplyr::select(date_time, baseline, crisis, eval(map_region_name), length, end_lon, end_lat) %>%
      rename("start_region" = eval(map_region_name)) %>%
      #drop all areas that don't fall within the region of interest
      subset(start_region != "<NA>" & !(is.na(start_region))) %>%
      mutate(end_lon = as.numeric(end_lon),
             end_lat = as.numeric(end_lat)) %>%
      #map end points to regions in map area
      st_as_sf(coords = c("end_lon", "end_lat"), crs = st_crs(map)) %>%
      st_join(map) %>%
      as_tibble() %>%
      dplyr::select(date_time, baseline, crisis, length, start_region, eval(map_region_name)) %>%
      rename("end_region" = eval(map_region_name)) %>%
      subset(end_region != "<NA>" & !(is.na(end_region))) -> mapped_data
  }
  
  
  if(read_from_cache){
    if(exists("mapped_data")){
      data <- rbind(data,mapped_data)
    }
  }else{
    data <- mapped_data
  }
  
  #save to cache
  if(save_to_cache){
    write_rds(data, paste0("../cache/", project_area,"/",project_name,".rds"))
  }

  return(list(data, map))
  
}


#reads in raw fb pop data, creates polygons and masks it to a region of interest
#VARS
#' @param path_to_map (string): relative or absolute path to map of interest, this should a map you want to mask the FB regions with
#' @param path_to_fb_pop_data (string): relative or absolute path to folder containing FB population data
#' @param tz (string): describe the timezone for the data run Olson() for a list of time zones
#' @param map_crs (integer): The EPSG number for the crs of interest 
#' @param read_from_cache (boolean): use local cache of previously processed data or run from scratch
#' @param save_to_cache (boolean): save output to local cache for subsequent use
#' @param project_area (string): describe the region that this project refers to 
#' @param projectname (string): the specific type of dataset you're working with for saving the cache

#temporary inputs left in for quick bug fixes
# path_to_map <- "../data/india/gis/adm0/IND_adm0.shp"
# path_to_fb_pop_data <- "../data/india/pop/"
# tz <- "Asia/Kolkata"
# map_crs <- 4326
# read_from_cache <- T
# save_to_cache <- T
# project_area <- "India"
# project_name <- "india_pop"

#convert and work with FB WKT for population data
load_fb_pop_map <- function(
  path_to_map,
  path_to_fb_pop_data,
  tz,
  map_crs = 4326,
  read_from_cache = F,
  save_to_cache = T,
  project_area, 
  project_name
  
){
  
  
  #error checking for input parameters
  if(read_from_cache & !file.exists(paste0("../cache/",project_area,"/",project_name,"map.rds"))){
    print(paste0("We were unable to find a cached file in: ", paste0("../cache/",project_area,"/",project_name,".rds")))
    print("Running function without reading cached data")
    read_from_cache <- F
  }
  if(!dir.exists(paste0("../cache/", project_area))){
    dir.create(paste0("../cache/", project_area), recursive = T, showWarnings = F)
  }
  if(!file.exists(path_to_map)){
    stop("Map file not found")
  }
  if(!dir.exists(path_to_fb_mvmt_data)){
    stop("Path to FB movement data doesn't exist")
  }
  if(list.files(path_to_fb_mvmt_data, pattern = "*?\\.csv$") %>% length == 0){
    stop("No csv files found in location of movement data")
  }
  
  if(read_from_cache & file.exists(paste0("../cache/", project_area,"/",project_name,"_map.rds"))){
    
    map <- read_rds(paste0("../cache/", project_area,"/",project_name,"_map.rds"))
    
  }else{
    
    ###creating spatial files from FB data
    data <- list.files(path_to_fb_pop_data)[1] %>% 
      {read_csv(paste0(path_to_fb_pop_data,.))}
    
    data %>% 
      pull(Geometry) %>% 
      st_as_sfc(crs = 4326) %>%
      st_sf %>%
      st_cast -> sp_data
    
    sp_data$`Spaco Id` <- data$`Spaco Id`
    
    #mask to india data 
    st_read(path_to_map) %>% #for multiple map areas
      st_transform(crs = map_crs) -> masking_map
    
    filter(sp_data, st_contains(masking_map, sp_data, sparse = FALSE)) -> map
    
  }
  
  if(save_to_cache){
    write_rds(map, paste0("../cache/", project_area,"/",project_name,"_map.rds"))
  }

  
  #load all data
  list.files(path = path_to_fb_pop_data, pattern = "*?\\.csv$", full.names = T) %>%
    lapply(function(x) read_csv(x, col_types = 
                                  cols_only(`Spaco Id`="c", 
                                            `Baseline: People` = "d", 
                                            `Crisis: People` = "i", 
                                            `Date Time` = "T"))) %>%
    bind_rows() %>%
    mutate("Date Time"=with_tz(`Date Time`, tz=tz)) %>%
    set_names(c("Spaco Id", "date_time", "baseline", "crisis")) -> data
  
  return(list(map, data))
  
}


