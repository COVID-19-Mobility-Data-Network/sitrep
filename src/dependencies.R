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
if(F){
  install.packages(package_list)
}

lapply(package_list, function(x) library(x, character.only = T))

rm(package_list)