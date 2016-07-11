# Installing and loading required packages
packages <- c("shiny", "magrittr", "sp")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}

if (require(leaflet) == FALSE) {
library(devtools)
install_github("fbaffie/leaflet")
}



library(shiny)
library(leaflet)
library(magrittr)  # For piping
library(sp)  # For the point.in.polygon function

## My modules: either load package or source modules from this directory
# library(ShinyModules)
source('map_modules.R')


##############################################

# EXAMPLES

# plotly filling exmaple

# library(plotly)
# p <- plot_ly(x = c(1, 2, 3, 4), y = c(0, 2, 3, 5), fill = "tozeroy")
# add_trace(p, x = c(1, 2, 3, 4), y = c(3, 5, 1, 7), fill = "tonexty")

