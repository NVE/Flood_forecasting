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

packages <- c("curl", "shiny", "magrittr", "sp", "plotly", "dplyr", "ggplot2", "lubridate", "leaflet")
ipak(packages)
# sp: For the point.in.polygon function

############################################################
# This section below could/should be cut and pasted into a makefile running on the webserver so that
# the model data gets updated everyday

# Loading NVEDATA to make sure I can update the data

if (!'devtools' %in% installed.packages()) {install.packages('devtools')}
library(devtools)
remove.packages('NVEDATA')  # Added this for the moment as the NVEDATA package may have been updated in the meantime
# To tidy up later by tracking the version number rather than uninstalling arbitrarily!
install_github("fbaffie/NVEDATA", ref = "florian")
# 
library(NVEDATA)
load_flood_data()

############################################################


## My modules: either load package or source modules from this directory
# library(ShinyModules)
source('map_modules.R')
source('plot_modules.R')
source('plotting_functions.R')
source('mapping_functions.R')

# Source all files from the RCura version of leaflet DOESNT WORK
# file.sources = list.files("./leaflet-plugins", pattern="*.R", full.names=TRUE)
# for (f in file.sources) {source(f) }

# Load the Rdata files that were prepared with the NVEDATA package.
# This creates the global variable
load("HBV_2014.RData")
load("HBV_2016.RData")
load("DDD.RData")
load("flomtabell.RData")
load("HBV_past_year.RData")
load("meta_data.rda")
hbv_catchments <- readLines("data/hbv_catchments.json") %>% paste(collapse = "\n")
station_numbers <- as.character(unique(HBV_2014$regine.main))  # All of the HBV_2016 and DD stations are in HBV_2014
station_names <- as.character(unique(HBV_2014$station.name))  # May not be optimal (if 2 stations had same name), but it works
station_nbname <- as.character(unique(HBV_2014$nbname))

## Metadata organized as below is needed for the maps.
# Maybe we can streamline with the rest later
station_indices <- which(meta_data$regine_main %in% station_numbers)  # 1 station in HBV_2014 is not in the metadata
stations <- lapply(meta_data, function(x) x[station_indices])
# stations$nbname_SPECIALCHAR <- paste(stations$regine_main, "-", stations$name, sep ="")
stations$nbname <- paste(stations$regine_main, "-", station_names[match(stations$regine_main, station_numbers)], sep ="")

# Calculation of a stations$flood_warning indicator for the forecast period
# I want to have it under the "stations" list for the moment as this list is used by the map functions
## WARNING: this first implementation has potential bugs and requires decisions on which variables o use!

HBV_2014_SimCorr <- dplyr::filter(HBV_2014, Type == "Runoff" & Variable == "SimCorr")
HBV_2014_SimCorr_maxed <- group_by(HBV_2014_SimCorr, nbname, regine.main) %>% dplyr::summarise(maxed = max(na.omit(Values)))

flom_obs1Y <- dplyr::filter(flomtabell, Type == "Obs" & Variable == "1Y") 

index_HBV <- match(stations$regine_main, HBV_2014_SimCorr_maxed$regine.main)
index_flomtabell <- match(HBV_2014_SimCorr_maxed$regine.main, flom_obs1Y$regine.main)

stations$flood_warning <- HBV_2014_SimCorr_maxed$maxed[index_HBV] / flom_obs1Y$Values[index_flomtabell[index_HBV]]


# library(shinyjs)

# jsResetCode <- "shinyjs.reset = function() {history.go(0)}"

