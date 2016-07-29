# This is the server logic for a Shiny web application.



server <- function(input, output) {
  
  # Below is the code to have a file load that updates itself, 
  # but it creates a reactive whih ends up being probably more trouble than usefulness...
  # HBV_2014_GG <<- reactiveFileReader(10000, session = NULL, filePath = "./HBV_2014_GG.Rdata", load, envir = .GlobalEnv)

  input4plot <- callModule(mapModule,"multimod_forecast_map")
  callModule(multimod_forecast_plot_mod,"multimod_forecast_plot", input4plot, HBV_2014, HBV_2016, DDD)
  
  input4plot_HBV_2014 <- callModule(mapModule,"map_HBV_2014")
  callModule(forecast_plot_mod,"forecast_plot_HBV_2014", input4plot_HBV_2014, HBV_2014)
  callModule(forecast_plot_mod_shading,"forecast_plot_shading_HBV_2014", input4plot_HBV_2014, HBV_2014)
  
  input4plot_HBV_2016 <- callModule(mapModule,"map_HBV_2016")
  callModule(forecast_plot_mod,"forecast_plot_HBV_2016", input4plot_HBV_2016, HBV_2016)
  callModule(forecast_plot_mod_shading,"forecast_plot_shading_HBV_2016", input4plot_HBV_2016, HBV_2016)
  
  input4plot_DDD <- callModule(mapModule,"map_DDD")
  callModule(forecast_plot_mod,"forecast_plot_DDD", input4plot_DDD, DDD)
  callModule(forecast_plot_mod_shading,"forecast_plot_shading_DDD", input4plot_DDD, DDD)

  callModule(mapModule_polygonFeature,"map_polygon")  

}
