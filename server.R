# This is the server logic for a Shiny web application.



server <- function(input, output, session) {
  
  # Below is the code to have a file load that updates itself, 
  # but it creates a reactive whih ends up being probably more trouble than usefulness...
  # HBV_2014_GG <<- reactiveFileReader(10000, session = NULL, filePath = "./HBV_2014_GG.Rdata", load, envir = .GlobalEnv)

  input4multimod_plot <- callModule(mapModule,"multimod_forecast_map")
  callModule(multimod_forecast_plot_mod,"multimod_forecast_plot", input4multimod_plot, HBV_2014, HBV_2016, DDD, HBV_past_year, flomtabell)
  
  # Let's try a tab that allows multiple station selection but with the same selector as the first tab
  input4multi_forecast_plot <- callModule(mapModule,"multistation_map")
  callModule(multimod_forecast_plot_mod,"multistation_plot", input4multi_forecast_plot, HBV_2014, HBV_2016, DDD, HBV_past_year, flomtabell)
  
  
  input4plot_HBV_2014 <- callModule(mapModule,"map_HBV_2014")
  callModule(forecast_plot_mod,"forecast_plot_HBV_2014", input4plot_HBV_2014, HBV_2014)
  
  input4plot_HBV_2016 <- callModule(mapModule,"map_HBV_2016")
  callModule(forecast_plot_mod,"forecast_plot_HBV_2016", input4plot_HBV_2016, HBV_2016)

  input4plot_DDD <- callModule(mapModule,"map_DDD")
  callModule(forecast_plot_mod,"forecast_plot_DDD", input4plot_DDD, DDD)

  callModule(OLD_mapModule_polygonFeature,"map_polygon")  
  
  callModule(table_mod,"metadata_table", meta_data) 
  callModule(table_mod,"RL_table", flomtabell) 
  callModule(table_mod,"HBV_2014_table", HBV_2014) 
  callModule(table_mod,"HBV_2016_table", HBV_2016) 
  callModule(table_mod,"DDD_table", DDD) 

  
  
}
