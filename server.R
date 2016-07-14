# This is the server logic for a Shiny web application.


source('global.R')

load("HBV_2014_GG.RData")

server <- function(input, output) {
  
  
  # HBV_2014_GG <<- reactiveFileReader(10000, session = NULL, filePath = "./HBV_2014_GG.Rdata", load, envir = .GlobalEnv)

  callModule(forecast_plot_mod,"forecast_plot") 
  
  map1_input <- callModule(mapModule_polygonFeature,"map1") 
  callModule(printoutModule,"map1_msg", map1_input)
  
  map2_input <- callModule(mapModule_polygonFeature,"map2")
  callModule(printoutModule,"map2_msg", map2_input)
  
    
}
