# This is the server logic for a Shiny web application.

server <- function(input, output) {
  
  # Below is the code to have a file load that updates itself, 
  # but it creates a reactive whih ends up being probably more trouble than usefulness...
  # HBV_2014_GG <<- reactiveFileReader(10000, session = NULL, filePath = "./HBV_2014_GG.Rdata", load, envir = .GlobalEnv)

#   map1_input <- callModule(mapModule_polygonFeature,"map1") 
#   callModule(printoutModule,"map1_msg", map1_input)
  
  input4plot <- callModule(mapModule,"map1")
  callModule(forecast_plot_mod,"forecast_plot", input4plot)
  callModule(forecast_plot_mod_shading,"forecast_plot_shading", input4plot)
  
}
