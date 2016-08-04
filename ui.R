# This is the user-interface definition of a Shiny web application.

ui <- navbarPage("Flomvarsling",  # cut off:  id = "nav",
                 
                 tabPanel("About", icon = icon("info"),
                          
                          fluidRow(
                            column(8, wellPanel(
                              HTML('
<p style="margin-left:1em; style="text-align:justify"> Blabla... flood data in Norway </p>')
                              )    
                            ))
                          
                 ),
                 tabPanel("Forecast - multi-model", icon = icon("random"),
                          mapModuleUI("multimod_forecast_map"),
                          multimod_forecast_selection_modUI("multimod_forecast_plot"),
                          forecast_plot_modUI("multimod_forecast_plot")
                 ),
                 navbarMenu("Forecast - single model", icon = icon("line-chart"),
                            tabPanel("HBV_2014",
                                     mapModuleUI("map_HBV_2014"),
                                     forecast_plot_modUI("forecast_plot_HBV_2014")
                                     # forecast_plot_mod_shadingUI("forecast_plot_shading_HBV_2014")
                                     ),
                            tabPanel("HBV_2016",
                                     mapModuleUI("map_HBV_2016"),
                                     forecast_plot_modUI("forecast_plot_HBV_2016")
                                     # forecast_plot_mod_shadingUI("forecast_plot_shading_HBV_2016")
                            ),
                            tabPanel("DDD",
                                     mapModuleUI("map_DDD"),
                                     forecast_plot_modUI("forecast_plot_DDD")
                                     # forecast_plot_mod_shadingUI("forecast_plot_shading_DDD")
                            )
                 ),
                 navbarMenu("Mapping tools", icon = icon("globe"),
                            tabPanel("Multi-station / single model",
                                     ## Commented: first intended way to do the multi-station multi-model tab
#                                      mapModule_polygonFeatureUI("map_polygon"),
#                                      multimod_forecast_selection_modUI("multi_plot")
                                     # forecast_plot_modUI("multi_plot")
                                     OLD_mapModule_polygonFeatureUI("map_polygon")
                                     )
#                             ,
#                             tabPanel("Map2")
                 ),
                navbarMenu("Historical tools", icon = icon("history"),
                            tabPanel("Past forecasting performance"
#                                      mapModuleUI("past_map"),
#                                      multimod_forecast_selection_modUI("past_plot"),
#                                      multimod_forecast_plot_modUI("past_plot")
                                    ),
                             tabPanel("Past events"),
                             tabPanel("Calibration results")
)
)
      


    