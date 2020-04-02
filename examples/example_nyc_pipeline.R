source("../src/dependencies.R")
source("../src/ingest_data.R")
source("../src/standard_plots.R")

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


#mung data
mvmt_output[[1]] %>%
  #extract the hour of the date_time variable
  mutate(time = as.integer(format(date_time, '%H'))) %>%
  #decide if you want to drop any time windows
  #subset(time > 3 & time < 18) %>%
  #convert date_time to date
  mutate(date = as_date(date_time)) %>%
  #convert km measurements into mi if needed
  mutate(length = conv_unit(length, "km", "mi")) %>%
  #create flag for travel inside and outside regions
  #calculate dist which is total distance traveled
  mutate(flag = ifelse(start_region == end_region, "Within NTA", "Between NTA"),
         dist = length*crisis) %>%
  #summarise values by date, start_region and flag
  group_by(date, start_region, flag) %>%
  summarise(baseline = sum(baseline),
            crisis = sum(crisis),
            dist = sum(dist)) %>%
  #calculate aggregated percent change
  mutate(perc_change = (crisis-baseline)/baseline) -> nyc_nta


generate_area_plots(data = nyc_nta,
                    map = mvmt_output[[2]],
                    map_region_name = "ntaname",
                    project_area = "NYC",
                    area_of_analysis = "city")

split(nyc_nta, nyc_nta$start_region) %>%
  lapply(function(x) generate_travel_plots(data = x,
                                           vector_data = mvmt_output[[1]],
                                           map = mvmt_output[[2]],
                                           map_region_name = "ntaname",
                                           project_area = "NYC",
                                           area_of_analysis = "nta",
                                           map_nested_under_name = "boro_name"))


rmarkdown::render("../templates/nyc_sitrep.Rmd", params = list(
  date = tail(nyc_nta$date,1)),
  output_format = "html_document",
  output_file = paste0("../output/nyc_sitrep/nyc_sitrep_",tail(nyc_nta$date,1),".html"))
