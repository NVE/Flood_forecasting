# This is the server logic for the Flood forecasting Shiny App.

server <- function(input, output, session) {
  
  input4multi_forecast_plot <- callModule(mapModule,"multistation_map")
  callModule(multimod_forecast_plot_mod,"multistation_plot", input4multi_forecast_plot, OBS, HBV_2014, HBV_2016, DDD, ODM, HBV_past_year, flomtabell)

  input4plot_HBV_2014 <- callModule(mapModule,"map_HBV_2014")
  callModule(forecast_plot_mod,"forecast_plot_HBV_2014", input4plot_HBV_2014, HBV_2014)
  
  input4plot_HBV_2016 <- callModule(mapModule,"map_HBV_2016")
  callModule(forecast_plot_mod,"forecast_plot_HBV_2016", input4plot_HBV_2016, HBV_2016)

  input4plot_DDD <- callModule(mapModule,"map_DDD")
  callModule(forecast_plot_mod,"forecast_plot_DDD", input4plot_DDD, DDD)
  
  input4plot_ODM <- callModule(mapModule,"map_ODM")
  callModule(forecast_plot_mod,"forecast_plot_ODM", input4plot_ODM, ODM)

## COMMENT: first intended way to do the multi-station multi-model tab with polygon selection. Did not work
# stations_model_vect <- callModule(mapModule_polygonFeature,"map_polygon") 
# callModule(multimod_forecast_plot_EXP, "multi_plot", "2.11", HBV_2014, HBV_2016, DDD)
  
  callModule(mapModule_polygonFeature,"map_polygon")  

  callModule(table_mod,"metadata_table", meta_data) 
  callModule(table_mod,"RL_table", flomtabell) 
  callModule(table_mod,"HBV_2014_table", HBV_2014) 
  callModule(table_mod,"HBV_2016_table", HBV_2016) 
  callModule(table_mod,"DDD_table", DDD) 
  callModule(table_mod,"ODM_table", ODM) 

}
