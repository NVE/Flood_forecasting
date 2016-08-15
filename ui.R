# This is the user-interface definition of a Shiny web application.

ui <- navbarPage("Flomvarsling", collapsible = TRUE, theme = "my_style.css",
                 
                 tabPanel("About", icon = icon("info"),
                          
                          fluidRow(
                            column(6, wellPanel(
                              HTML('
<p style="margin-left:1em; style="text-align:justify"> This is florians dev branch of the app: flood data in Norway.</p>
<p style="margin-left:1em; style="text-align:justify"> If you find bugs or want to request new features, please file them                              
                              <a href="https://github.com/fbaffie/Flood_forecasting/issues" target="_blank">on GitHub</a></p>
 <p style="margin-left:1em; style="text-align:justify"> The to-do list for this project can be consulted                              
                              <a href="https://trello.com/b/KrVO7fEq" target="_blank">online</a> and shows what is currently in development.</p>  
<p style="margin-left:1em; style="text-align:justify"> The app is structed as follows:</p>

                         <p style="margin-left:1em; style="text-align:justify"> <big> <b> Multi-station and multi-model tab </b> </big> </p>
                          <div> <dl>
                        <p style="margin-left:1em; style="text-align:justify"> Select multiple stations </p>
                        <p style="margin-left:1em; style="text-align:justify"> Select which type of variable to plot: input, discharge or state variables </p>
                        <p style="margin-left:1em; style="text-align:justify"> Select which variables to plot </p>
                        <p style="margin-left:1em; style="text-align:justify"> The map features selectable layers with both a flood warning indicator and a measure of model uncertainty </p>
                          </div> </dl>                          
                        <p style="margin-left:1em; style="text-align:justify"> <b> Model performance on past forecast dsta and calibration data </b> </p>
                          <p style="margin-left:1em; style="text-align:justify"> At present, 1 year of past forecast is available with HBV_2014 </p>
                                   ')
                              )),
                            column(6,
                              img(src='flood.jpg', align = "right")
                            )
                            ),
                          fluidRow(
                            column(6, wellPanel(
                              HTML('
<p style="margin-left:1em; style="text-align:justify"> This is florians dev branch of the app: flood data in Norway.</p>
                      <p style="margin-left:1em; style="text-align:justify"> At present, 1 year of past forecast is available with HBV_2014 </p>
                                   ')
                            )),
                            column(6,
                                   img(src='multimodel_tab_with_warning.png', align = "right")
                          ))
                          
                          
                 ),
                 navbarMenu("Multi-station / Multi-model", icon = icon("random"),
                  tabPanel("DropDown station selection", 
                          mapModuleUI("multistation_map", multiple = TRUE),
                          multimod_forecast_selection_modUI("multistation_plot"),
                          forecast_plot_modUI("multistation_plot")
                  ),
                  tabPanel("Polygon selection",
                           ## Commented: first intended way to do the multi-station multi-model tab
         #                                      mapModule_polygonFeatureUI("map_polygon"),
         #                                      multimod_forecast_selection_modUI("multi_plot")
         # forecast_plot_modUI("multi_plot")
                          OLD_mapModule_polygonFeatureUI("map_polygon")
                  )),
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
      


    