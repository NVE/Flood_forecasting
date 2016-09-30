setwd("D:/Github/Flood_forecasting")

############################################################################################################
## Installing and loading required packages (https://gist.github.com/stevenworthington/3178163)
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

packages <- c("curl", "shiny", "magrittr", "sp", "plotly", "dplyr", "ggplot2", "lubridate", "leaflet", "shinyBS", "DT")
ipak(packages)
# sp: For the point.in.polygon function

############################################################################################################
## Loading NVEDATA to make sure I can update the data

if (!'devtools' %in% installed.packages()) {install.packages('devtools')}
library(devtools)
remove.packages('NVEDATA')  # Added this for the moment as the NVEDATA package may have been updated in the meantime
# To tidy up later by tracking the version number rather than uninstalling arbitrarily!
install_github("fbaffie/NVEDATA")

library(NVEDATA)

load_flood_data()
############################################################################################################

# stopApp()
# runGitHub("Flood_forecasting", "fbaffie", ref = "operational", launch.browser = TRUE)



# runApp(launch.browser = TRUE)

# http://127.0.0.1:3921/


# unlink("reports", recursive = TRUE, force = FALSE)
# rmarkdown::render("apple_microsoft.Rmd", "all", output_dir = "reports")

# library(gmailr)
# gmailr::send_message(mime(from="fbaffie@gmail.com", to="flbk@nve.no",
#                           subject="Updating FlomVarsling data", "Seems to run fine! find it there http://127.0.0.1:4148/"))
# Error: oauth_listener() needs an interactive environment.
# Execution halted


