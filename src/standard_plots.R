source("../src/dependencies.R")

#functions to generate standard plots for reports

#area summary function

#generates whole area plots from cleaned data
#VARS
#' @param data (tibble): munged output from ingest file
#' @param map (sf object): sf object used to subset data
#' @param map_region_name (string): the unique variable name in "map" that you want to map your data to
#' @param project_area (string): The area that you are looking at india/nyc/etc...
#' @param area_of_analysis (string): The type of region you're looking at city/district/county/etc...
# input variables for testing and debugging
# data <- nyc_nta
# map <- mvmt_output[[2]]
# map_region_name <- "ntaname"
# project_area <- "NYC"
# area_of_analysis <- "city"

generate_area_plots <- function(
  data,
  map,
  map_region_name,
  project_area,
  area_of_analysis
){
  
  data %>%
    subset(date == tail(unique(data$date),1)) %>%
    {merge(map, .,by.x = eval(map_region_name), by.y = "start_region", all.y = T)} %>%
    ggplot() + 
    geom_sf(data = map, fill = "white", color = "gray") + 
    geom_sf(aes(fill = perc_change)) +
    facet_wrap(.~flag) +
    scale_fill_gradient2(labels = scales::percent) +
    theme_bw() +
    theme(axis.title=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank(), 
          legend.position = "left") +
    labs(fill = "Percent Change", 
         title = paste("Percent Change in # of trips - ", project_area, " - ",
                       format(tail(sort(data$date),1), "%a, %b %d"))) -> a
  
  
  data %>%
    group_by(date, flag) %>%
    summarise(baseline = sum(baseline), crisis = sum(crisis)) %>%
    mutate(perc_change = (crisis-baseline)/baseline) %>%
    ggplot() + 
    geom_line(aes(x = date, y = perc_change))+ 
    scale_y_continuous(labels=scales::percent) +
    facet_wrap(.~flag)+
    theme_bw() + 
    labs(x = "Date", y = "Percent change", 
         title = paste0("Percent change in the total number of trips made compared to baseline")) -> b1
  
  
  data %>%
    group_by(date, flag) %>%
    summarise(dist = sum(dist)) %>%
    ggplot() + 
    geom_line(aes(x = date, y = dist)) + 
    scale_y_log10(labels = scales::comma)+
    theme_bw() + 
    facet_wrap(.~flag) +
    labs(x = "Date", 
         y = "Distance (miles)", 
         title = paste0("Total distance traveled in those trips")) -> b2
  

  
  b <- ggarrange(b1, b2, ncol = 1, align = "v")
  
  out_plot <- ggarrange(a, b, ncol = 1)
  
  paste0("../figs/", project_area,"/", tail(sort(data$date),1), "/", area_of_analysis, "/") %>%
    lapply(function(x) dir.create(x, recursive = T, showWarnings = F))
  
  suppressMessages(
    ggsave(filename = paste0("../figs/", project_area,"/", tail(sort(data$date),1), "/",area_of_analysis,"/",project_area,".png"), 
           plot = out_plot,
           dpi = 300)
  ) 
  
  
}


#generates whole area plots from cleaned data
#VARS
#' @param data (tibble): munged output from ingest file
#' @param map (sf object): sf object used to subset data
#' @param map_region_name (string): the unique variable name in "map" that you want to map your data to
#' @param project_area (string): The area that you are looking at india/nyc/etc...
#' @param area_of_analysis (string): The type of region you're looking at city/district/county/etc...
#' @param map_nested_under_name (string): The admin area that this admin area is nested under
# input variables for testing and debugging
 # data <- nyc_nta %>% subset(start_region == unique(nyc_nta$start_region)[1])
 # vector_data <- mvmt_output[[1]]
 # map <- mvmt_output[[2]]
 # map_region_name <- "ntaname"
 # project_area <- "NYC"
 # area_of_analysis <- "nta"
 # map_nested_under_name <- "boro_name"
 
 
generate_travel_plots <- function(
  data, 
  vector_data,
  map, 
  map_region_name, 
  project_area, 
  area_of_analysis,
  map_nested_under_name = NA
){
  
  if(nrow(data) > 0){
    
    last_date <- tail(data$date, 1)
    area_of_interest <- unique(data$start_region)
    
    print(area_of_interest)
    
    #calculate regions of travel out
    vector_data %>%
      mutate(date = as_date(date_time)) %>%
      subset(start_region == area_of_interest &
               start_region != end_region & 
               date == last_date) %>%
      group_by(end_region) %>%
      summarise(crisis = sum(crisis),
                baseline = sum(baseline)) %>%
      arrange(-crisis) %>% 
      head(5) %>%
      set_names(c("Locations Out", "Travel Today", "Travel Baseline")) -> map_tmp_out
    
    #calculate regions of travel in
    vector_data %>%
      mutate(date = as_date(date_time)) %>%
      subset(end_region == area_of_interest &
               start_region != end_region & 
               date == last_date) %>%
      group_by(start_region) %>%
      summarise(crisis = sum(crisis),
                baseline = sum(baseline)) %>%
      arrange(-crisis) %>% 
      head(5) %>%
      set_names(c("Locations In", "Travel Today", "Travel Baseline")) -> map_tmp_in
    
    #both maps 
    c(as.character(map_tmp_in[map_tmp_in$`Locations In` %in% map_tmp_out$`Locations Out`,]$`Locations In`),
      as.character(map_tmp_out[map_tmp_out$`Locations Out` %in% map_tmp_in$`Locations In`,]$`Locations Out`)) %>%
      unique() %>%
      as.data.frame() %>%
      set_names("Both") -> map_tmp_both
    
    if(nrow(map_tmp_out)==0){
      map_tmp_out <- rbind(map_tmp_out, data.frame("Locations Out" = "No FB travel out"))
    }
    
    if(nrow(map_tmp_in)==0){
      map_tmp_in <- rbind(map_tmp_in, data.frame("Locations In" = "No FB travel in"))
    }
    
    aoi_map <- filter(map, !!as.symbol(map_region_name) == area_of_interest)
    in_map <- merge(map, map_tmp_in, by.x = eval(map_region_name), by.y = "Locations In")
    out_map <- merge(map, map_tmp_out, by.x = eval(map_region_name), by.y = "Locations Out")
    both_map <- merge(map, map_tmp_both, by.x = eval(map_region_name), by.y = "Both")
    all_names_map <- rbind(in_map, out_map) %>% dplyr::select(eval(map_region_name)) %>% distinct(.keep_all = T)
    
    ggplot() + 
      geom_sf(data = in_map, fill = "blue", color = "gray") + 
      geom_sf(data = out_map, fill = "red", color = "gray") +
      geom_sf(data = both_map, fill = "darkgreen", color = "gray") +
      geom_sf(data = aoi_map, fill = "gray", color = "black") + 
      ggrepel::geom_label_repel(data = all_names_map,
                                aes_string(label = map_region_name, geometry = "geometry"),
                                stat = "sf_coordinates",
                                min.segment.length = 0, fontface = "bold", size = 2) +
      theme_bw() +
      theme(axis.title=element_blank(),
            axis.text=element_blank(),
            axis.ticks=element_blank()) +
      labs(caption = paste("Travel network on",format(last_date, "%a, %b %d"),
                           "\nRED - Travel into", area_of_interest,
                           "\nBLUE - Travel out of", area_of_interest,
                           "\nGREEN - Both")) -> a
    
    
    tables <- ggarrange(ggtexttable(map_tmp_out, rows = NULL, 
                                    theme = ttheme("mRed", base_size = 8)) + theme(plot.margin = margin(0,0,0,0,"pt")),
                        ggtexttable(map_tmp_in, rows = NULL, 
                                    theme = ttheme("mBlue", base_size = 8)) + theme(plot.margin = margin(0,0,0,0,"pt")),
                        ncol = 2)
    
    total_travel <- sum(c(map_tmp_in$`Travel Today`,map_tmp_out$`Travel Today`))
    
    data %>%
      group_by(date, flag) %>%
      summarise(baseline = sum(baseline), crisis = sum(crisis)) %>%
      mutate(perc_change = (crisis-baseline)/baseline) %>%
      ggplot() + 
      geom_line(aes(x = date, y = perc_change), size = 1.05)+ 
      scale_y_continuous(labels=scales::percent) +
      facet_wrap(.~flag)+
      theme_bw() + 
      labs(x = "Date", y = "Percent change", 
           title = paste0("Percent change in the total number of trips made compared to baseline")) -> b1
    
    
    data %>%
      group_by(date, flag) %>%
      summarise(dist = sum(dist)) %>%
      ggplot() + 
      geom_line(aes(x = date, y = dist), size = 1.05) + 
      scale_y_log10(labels = comma)+
      theme_bw() + 
      facet_wrap(.~flag) +
      labs(x = "Date", 
           y = "Distance (miles)", 
           title = paste0("Total distance traveled in those trips")) -> b2
    
    b <- ggarrange(b1, b2, ncol = 1, align = "v")
    
    district_out <- ggarrange(a, tables, b, ncol = 1) %>% 
      annotate_figure(top = text_grob(paste(toupper(area_of_analysis), "- level analysis"), face = "bold", size = 14))
    
    ifelse(is.na(map_nested_under_name),
           paste0("../figs/", project_area,"/", tail(sort(data$date),1), "/",area_of_analysis,"/",
                  total_travel," - ",
                  area_of_interest,".png"),
           paste0("../figs/", project_area,"/", tail(sort(data$date),1), "/",area_of_analysis,"/",
                  total_travel," - ",
                  filter(map, !!as.symbol(map_region_name) == area_of_interest) %>% pull(map_nested_under_name)," - ",
                  area_of_interest,".png")) -> out_filename
    
    paste0("../figs/", project_area,"/", tail(sort(data$date),1), "/", area_of_analysis, "/") %>%
      lapply(function(x) dir.create(x, recursive = T, showWarnings = F))
    
    suppressMessages(
      ggsave(filename = out_filename, 
             plot = district_out, dpi = 300, height = 11, width = 8.5, units = "in")
    ) 
    
  }else{
    NULL
  }
  
}

#Pop area summary function

#generates whole area plots from cleaned data
#VARS
#' @param data (tibble): munged output from ingest file
#' @param map (sf object): sf object used to subset data
#' @param map_region_name (string): the unique variable name in "map" that you want to map your data to
#' @param project_area (string): The area that you are looking at india/nyc/etc...
#' @param area_of_analysis (string): The type of region you're looking at city/district/county/etc...
# input variables for testing and debugging
 # data <- pop_output[[1]]
 # map <- pop_output[[2]]
 # map_region_name <- "NAME10"
 # project_area <- "Syracuse"
 # area_of_analysis <- "city"

generate_syracuse_area_plots <- function(
  data,
  map,
  map_region_name,
  project_area,
  area_of_analysis
){
  
  ggplot(data = map) + 
    geom_sf() + 
    geom_sf_text(aes(label = NAME10)) +
    theme_bw() +
    theme(axis.title=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank()) +
    labs(title = "Census Tracts in Syracuse") -> a
  
  data %>%
    group_by(date_time, region) %>%
    summarise(baseline = sum(n_baseline, na.rm = T), crisis = sum(n_crisis, na.rm = T)) %>%
    ungroup() %>%
    mutate(date_time = as_date(date_time)) %>%
    group_by(date_time, region) %>%
    summarise(baseline = mean(baseline), crisis = mean(crisis)) %>%
    subset(date_time %in% tail(sort(unique(date_time)),6)) %>%
    ungroup() %>%
    mutate(perc_change = (crisis-baseline)/baseline) %>%
  {merge(map, .,by.x = eval(map_region_name), by.y = "region", all.y = T)} %>%
    ggplot() + 
    geom_sf(data = map, fill = "white", color = "gray") + 
    geom_sf(aes(fill = perc_change)) +
    facet_wrap(.~date_time, nrow = 2) +
    scale_fill_gradient2(labels = scales::percent, high = "red", low = "blue") +
    theme_bw() +
    theme(axis.title=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank(), 
          legend.position = "bottom") +
    labs(fill = "Percent Change", 
         title = "Percent change in FB population compared to baseline ") -> b
  
  
  data %>%
    subset(as_date(date_time) > as_date("2020-04-05")) %>%
    mutate(tod = hour(date_time) %>% factor(labels = c("4AM - 12PM", "12PM - 8PM", "8PM - 4AM"))) %>% 
    group_by(date_time, tod) %>%
    summarise(baseline = sum(n_baseline, na.rm = T),
              crisis = sum(n_crisis, na.rm = T)) %>%
    mutate(perc_change = (crisis - baseline)/baseline) %>%
    mutate(weekend = wday(date_time, label = T),
           weekend = ifelse(weekend %in% c("Sun", "Sat"), "Weekend", "Weekday")) %>%
    ggplot(aes(x = date_time, y = perc_change, color = tod)) + 
    geom_line(size = 1.05) + 
    geom_point(aes(shape = weekend), size = 3) + 
    geom_hline(yintercept = 0, linetype = 2) + 
    scale_y_continuous(labels = scales::percent) + 
    theme_bw() +
    labs(x = "Date Time", y = "Percent Change", color = "Time of Day", shape = "Day of Week") -> c
  

  
  data %>%
    subset(as_date(date_time) > as_date("2020-04-05")) %>%
    mutate(tod = hour(date_time) %>% factor(labels = c("4AM - 12PM", "12PM - 8PM", "8PM - 4AM"))) %>% 
    group_by(date_time, tod, region) %>%
    summarise(baseline = sum(n_baseline, na.rm = T),
              crisis = sum(n_crisis, na.rm = T)) %>%
    mutate(perc_change = (crisis - baseline)/baseline) %>%
    mutate(weekend = wday(date_time, label = T),
           weekend = ifelse(weekend %in% c("Sun", "Sat"), "Weekend", "Weekday")) %>%
    ggplot(aes(x = date_time, y = perc_change, color = tod)) + 
    geom_line() + 
    geom_hline(yintercept = 0, linetype = 2) + 
    scale_y_continuous(labels = scales::percent) + 
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1)
    ) + 
    labs(x = "Date Time", y = "Percent Change", color = "Time of Day", shape = "Day of Week") + 
    facet_wrap_paginate(~region, ncol = 3, nrow = 5, page = 1) -> d
  
  data %>%
    subset(as_date(date_time) > as_date("2020-04-05")) %>%
    mutate(tod = hour(date_time) %>% factor(labels = c("4AM - 12PM", "12PM - 8PM", "8PM - 4AM"))) %>% 
    group_by(date_time, tod, region) %>%
    summarise(baseline = sum(n_baseline, na.rm = T),
              crisis = sum(n_crisis, na.rm = T)) %>%
    mutate(perc_change = (crisis - baseline)/baseline) %>%
    mutate(weekend = wday(date_time, label = T),
           weekend = ifelse(weekend %in% c("Sun", "Sat"), "Weekend", "Weekday")) %>%
    ggplot(aes(x = date_time, y = perc_change, color = tod)) + 
    geom_line() + 
    geom_hline(yintercept = 0, linetype = 2) + 
    scale_y_continuous(labels = scales::percent) + 
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1)
    ) + 
    labs(x = "Date Time", y = "Percent Change", color = "Time of Day", shape = "Day of Week") + 
    facet_wrap_paginate(~region, ncol = 3, nrow = 5, page = 2) -> e
  
  data %>%
    subset(as_date(date_time) > as_date("2020-04-05")) %>%
    mutate(tod = hour(date_time) %>% factor(labels = c("4AM - 12PM", "12PM - 8PM", "8PM - 4AM"))) %>% 
    group_by(date_time, tod, region) %>%
    summarise(baseline = sum(n_baseline, na.rm = T),
              crisis = sum(n_crisis, na.rm = T)) %>%
    mutate(perc_change = (crisis - baseline)/baseline) %>%
    mutate(weekend = wday(date_time, label = T),
           weekend = ifelse(weekend %in% c("Sun", "Sat"), "Weekend", "Weekday")) %>%
    ggplot(aes(x = date_time, y = perc_change, color = tod)) + 
    geom_line() + 
    geom_hline(yintercept = 0, linetype = 2) + 
    scale_y_continuous(labels = scales::percent) + 
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1)
    ) + 
    labs(x = "Date Time", y = "Percent Change", color = "Time of Day", shape = "Day of Week") + 
    facet_wrap_paginate(~region, ncol = 3, nrow = 5, page = 3) -> f
  
  data %>%
    subset(as_date(date_time) > as_date("2020-04-05")) %>%
    mutate(tod = hour(date_time) %>% factor(labels = c("4AM - 12PM", "12PM - 8PM", "8PM - 4AM"))) %>% 
    group_by(date_time, tod, region) %>%
    summarise(baseline = sum(n_baseline, na.rm = T),
              crisis = sum(n_crisis, na.rm = T)) %>%
    mutate(perc_change = (crisis - baseline)/baseline) %>%
    mutate(weekend = wday(date_time, label = T),
           weekend = ifelse(weekend %in% c("Sun", "Sat"), "Weekend", "Weekday")) %>%
    ggplot(aes(x = date_time, y = perc_change, color = tod)) + 
    geom_line() + 
    geom_hline(yintercept = 0, linetype = 2) + 
    scale_y_continuous(labels = scales::percent) + 
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1)
    ) + 
    labs(x = "Date Time", y = "Percent Change", color = "Time of Day", shape = "Day of Week") + 
    facet_wrap_paginate(~region, ncol = 3, nrow = 5, page = 4) -> g
  
  paste0("../figs/", project_area,"/", tail(sort(as_date(data$date_time)),1), "/", area_of_analysis, "/") %>%
    lapply(function(x) dir.create(x, recursive = T, showWarnings = F))
  
  suppressMessages(
    ggsave(filename = paste0("../figs/", project_area,"/", tail(sort(as_date(data$date_time)),1), "/",area_of_analysis,"/",project_area,"-ct.png"), 
           plot = a,
           dpi = 300)
  ) 
  suppressMessages(
    ggsave(filename = paste0("../figs/", project_area,"/", tail(sort(as_date(data$date_time)),1), "/",area_of_analysis,"/",project_area,"-heatmap.png"), 
           plot = b,
           dpi = 300)
  ) 
  suppressMessages(
    ggsave(filename = paste0("../figs/", project_area,"/", tail(sort(as_date(data$date_time)),1), "/",area_of_analysis,"/",project_area,"-perc_change.png"), 
           plot = c,
           dpi = 300)
  ) 
  suppressMessages(
    ggsave(filename = paste0("../figs/", project_area,"/", tail(sort(as_date(data$date_time)),1), "/",area_of_analysis,"/",project_area,"-ct-perc_change-1.png"), 
           plot = d,
           dpi = 300)
  ) 
  suppressMessages(
    ggsave(filename = paste0("../figs/", project_area,"/", tail(sort(as_date(data$date_time)),1), "/",area_of_analysis,"/",project_area,"-ct-perc_change-2.png"), 
           plot = e,
           dpi = 300)
  ) 
  suppressMessages(
    ggsave(filename = paste0("../figs/", project_area,"/", tail(sort(as_date(data$date_time)),1), "/",area_of_analysis,"/",project_area,"-ct-perc_change-3.png"), 
           plot = f,
           dpi = 300)
  ) 
  suppressMessages(
    ggsave(filename = paste0("../figs/", project_area,"/", tail(sort(as_date(data$date_time)),1), "/",area_of_analysis,"/",project_area,"-ct-perc_change-4.png"), 
           plot = g,
           dpi = 300)
  ) 
  
  
}



