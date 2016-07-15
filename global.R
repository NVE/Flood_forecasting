# Installing and loading required packages (https://gist.github.com/stevenworthington/3178163)
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, library, character.only = TRUE)
}


## Special case for leaflet which comes from a fork of Rcura on my repo
packages <- c("leaflet")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages('devtools')
  library(devtools)
  install_github("fbaffie/leaflet")
}


packages <- c("shiny", "leaflet", "magrittr", "sp", "plotly", "dplyr", "ggplot2", "lubridate")
ipak(packages)
# sp: For the point.in.polygon function


## My modules: either load package or source modules from this directory
# library(ShinyModules)
source('map_modules.R')
source('plot_modules.R')
source('plotting_functions.R')


# Load the Rdata files that were prepared with the NVEDATA package.
# This creates the global variable
load("HBV_2014.RData")
load("HBV_2016.RData")
load("meta_data.rda")
stations_available <- as.character(unique(HBV_2014$regine_main))
stations_index <- which(meta_data$regine_main %in% stations_available)

## Metadata organized as below is needed for the maps.
# Maybe we can streamline with the rest later
stations <- list()
stations$regine_main <- meta_data$regine_main[stations_index]
stations$name <- meta_data$station_name[stations_index]
stations$long <- meta_data$longitude[stations_index]
stations$lat <- meta_data$latitude[stations_index]

# test <- meta_data[[1:80]][stations_index]  # doesn't work. something similar would be good for a subset of metadata



