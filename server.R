# This is the server logic for a Shiny web application.

# Load the Rdata files that were prepared with the NVEDATA package.
# This creates the global variable
load("HBV_2014_GG.RData")

server <- function(input, output) {
  
  # Below is the code to have a file load that updates itself, 
  # but it creates a reactive whih ends up being probably more trouble than usefulness...
  # HBV_2014_GG <<- reactiveFileReader(10000, session = NULL, filePath = "./HBV_2014_GG.Rdata", load, envir = .GlobalEnv)

  callModule(forecast_plot_mod,"forecast_plot") 
  
  map1_input <- callModule(mapModule_polygonFeature,"map1") 
  callModule(printoutModule,"map1_msg", map1_input)
  
  map2_input <- callModule(mapModule_polygonFeature,"map2")
  callModule(printoutModule,"map2_msg", map2_input)
  
    
}
