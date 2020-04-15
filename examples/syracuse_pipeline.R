source("../src/dependencies.R")
source("../src/ingest_data.R")
source("../src/standard_plots.R")

#map all movement data from FB to 
map_fb_pop(path_to_map = "../data/syracuse/gis/Census_Tracts_in_Syracuse_NY_2010.shp",
                path_to_fb_mvmt_data = "../data/syracuse/pop/",
                tz = "US/Eastern",
                map_region_name = "NAME10",
                map_crs = 4326,
                read_from_cache = T,
                save_to_cache = T,
                project_area = "NYC",
                project_name = "pop") -> pop_output

#generate syracuse plots 
generate_syracuse_area_plots(
  data = pop_output[[1]], 
  map = pop_output[[2]], 
  map_region_name = "NAME10",
  project_area = "Syracuse", 
  area_of_analysis = "city"
)

rmarkdown::render("../templates/syracuse_sitrep.Rmd", params = list(
  date = tail(sort(as_date(pop_output[[1]]$date_time)),1)),
  output_format = "pdf_document",
  output_file = paste0("../output/syracuse_sitrep/syracuse_sitrep_",tail(sort(as_date(pop_output[[1]]$date_time)),1),".pdf"))
