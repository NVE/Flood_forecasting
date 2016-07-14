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



