# Installing and loading required packages (https://gist.github.com/stevenworthington/3178163)
# ipak <- function(pkg){
#   new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
#   if (length(new.pkg)) 
#     install.packages(new.pkg, dependencies = TRUE)
#   sapply(pkg, library, character.only = TRUE)
# }


## Special case for leaflet which comes from a fork of Rcura on my repo
# packages <- c("leaflet")
# if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
#   install.packages('devtools')
#   library(devtools)
#   install_github("fbaffie/leaflet")
# }

# packages <- c("curl", "shiny", "magrittr", "sp", "plotly", "dplyr", "ggplot2", "lubridate", "leaflet")
# ipak(packages)
# sp: For the point.in.polygon function

############################################################
# This section below could/should be cut and pasted into a makefile running on the webserver so that
# the model data gets updated everyday

# Loading NVEDATA to make sure I can update the data

# if (!'devtools' %in% installed.packages()) {install.packages('devtools')}
# library(devtools)
# remove.packages('NVEDATA')  # Added this for the moment as the NVEDATA package may have been updated in the meantime
# # To tidy up later by tracking the version number rather than uninstalling arbitrarily!
# install_github("fbaffie/NVEDATA", ref = "shiny_compatible")
# 
# library(NVEDATA)
# 
# load_flood_data()

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

load(paste(getwd(), "/HBV_2014.RData", sep = ""))
load(paste(getwd(), "/HBV_2016.RData", sep = ""))
load(paste(getwd(), "/DDD.RData", sep = ""))
load(paste(getwd(), "/flomtabell.RData", sep = ""))
load(paste(getwd(), "/HBV_past_year.RData", sep = ""))
load(paste(getwd(), "/meta_data.rda", sep = ""))


# load("HBV_2014.RData")
# load("HBV_2016.RData")
# load("DDD.RData")
# load("flomtabell.RData")
# load("HBV_past_year.RData")
# load("meta_data.rda")
hbv_catchments <- readLines("data/hbv_catchments.json") %>% paste(collapse = "\n")
station_numbers <- as.character(unique(HBV_2014$regine.main))  # All of the HBV_2016 and DD stations are in HBV_2014
station_names <- as.character(unique(HBV_2014$station.name))  # May not be optimal (if 2 stations had same name), but it works
station_nbname <- as.character(unique(HBV_2014$nbname))


## Metadata organized as below is needed for the maps.
# Maybe we can streamline with the rest later
station_indices <- which(meta_data$regine_main %in% station_numbers)  # 1 station in HBV_2014 is not in the metadata
stations <- list()
stations$regine_main <- meta_data$regine_main[station_indices]
stations$name <- meta_data$station_name[station_indices]
stations$nbname <- paste(stations$regine_main, "-", stations$name, sep ="")
stations$long <- meta_data$longitude[station_indices]
stations$lat <- meta_data$latitude[station_indices]


# library(shinyjs)

# jsResetCode <- "shinyjs.reset = function() {history.go(0)}"

