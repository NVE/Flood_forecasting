# This is the user-interface definition of a Shiny web application.

ui <- navbarPage("Flomvarsling", 
                 
                 tabPanel("About", icon = icon("info"),
                          
                          fluidRow(
                            column(12, wellPanel(
                              HTML('
<p style="margin-left:1em; style="text-align:justify"> This is florians dev branch of the app: flood data in Norway.</p>
<p style="margin-left:1em; style="text-align:justify"> If you find bugs or want to request new features, please file them                              
                              <a href="https://trello.com/invite/b/KrVO7fEq/ae340c8f77522811cfe30dccf45b3e14/flood-forecasting-app" target="_blank">there</a></p>')
                              ),
#                               helpText(   a("If you find bugs or want to request new features, please file them there",
#                                             href="https://trello.com/invite/b/KrVO7fEq/ae340c8f77522811cfe30dccf45b3e14/flood-forecasting-app")),
                              
                              img(src='flood.jpg', align = "right")
                              # img(src='multimodel_tab_with_warning.png', align = "right")

                            ))
                          
                 ),
                  tabPanel("Multi-station-model / DropDown selector", icon = icon("random"),
                          mapModuleUI("multistation_map", multiple = TRUE),
                          multimod_forecast_selection_modUI("multistation_plot"),
                          forecast_plot_modUI("multistation_plot")
                  ),
                  tabPanel("Multi-station-model / Polygon",
                           ## Commented: first intended way to do the multi-station multi-model tab
         #                                      mapModule_polygonFeatureUI("map_polygon"),
         #                                      multimod_forecast_selection_modUI("multi_plot")
         # forecast_plot_modUI("multi_plot")
                          OLD_mapModule_polygonFeatureUI("map_polygon")
                  ),
                 navbarMenu("Forecast - single model", icon = icon("line-chart"),
                            tabPanel("HBV_2014",
                                     mapModuleUI("map_HBV_2014"),
                                     OLD_forecast_plot_modUI("forecast_plot_HBV_2014")
                                     # forecast_plot_mod_shadingUI("forecast_plot_shading_HBV_2014")
                                     ),
                            tabPanel("HBV_2016",
                                     mapModuleUI("map_HBV_2016"),
                                     OLD_forecast_plot_modUI("forecast_plot_HBV_2016")
                                     # forecast_plot_mod_shadingUI("forecast_plot_shading_HBV_2016")
                            ),
                            tabPanel("DDD",
                                     mapModuleUI("map_DDD"),
                                     OLD_forecast_plot_modUI("forecast_plot_DDD")
                                     # forecast_plot_mod_shadingUI("forecast_plot_shading_DDD")
                            )
                 ),
                 navbarMenu("Mapping tools", icon = icon("globe")

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
      


    