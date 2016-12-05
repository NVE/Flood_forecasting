# This is the user-interface definition of a Shiny web application.

# test if shinyserver responds

ui <- navbarPage(title = HTML("<a href=\"http://NVE.github.io/Flood_forecasting\">Flomvarsling</a>"), collapsible = TRUE, theme = "my_style.css",
                 
                 navbarMenu("Multistasjon / Multimodell", icon = icon("random"),
                            
                            tabPanel("Velg stasjoner med nedtrekksmeny", 
                                     mapModuleUI("multistation_map", multiple = TRUE),
                                     multimod_forecast_selection_modUI("multistation_plot"),
                                     forecast_plot_modUI("multistation_plot")
                            ),
                            tabPanel("Velg stasjoner med polygon",
                                     ## Commented: first intended way to do the multi-station multi-model tab
                                     #                                      mapModule_polygonFeatureUI("map_polygon"),
                                     #                                      multimod_forecast_selection_modUI("multi_plot")
                                     # forecast_plot_modUI("multi_plot")
                                     mapModule_polygonFeatureUI("map_polygon")
                            )),
                 
                 navbarMenu("Enkeltstasjon / Enkeltmodell", icon = icon("line-chart"),
                            tabPanel("HBV med usikkerhetsmodell",
                                     mapModuleUI("map_HBV_2014"),
                                     singlemodel_forecast_plot_modUI("forecast_plot_HBV_2014")
                                     # forecast_plot_mod_shadingUI("forecast_plot_shading_HBV_2014")
                            ),
                            tabPanel("HBV med +/- 50% nedbør",
                                     mapModuleUI("map_HBV_2016"),
                                     singlemodel_forecast_plot_modUI("forecast_plot_HBV_2016")
                                     # forecast_plot_mod_shadingUI("forecast_plot_shading_HBV_2016")
                            ),
                            tabPanel("DDD",
                                     mapModuleUI("map_DDD"),
                                     singlemodel_forecast_plot_modUI("forecast_plot_DDD")
                                     # forecast_plot_mod_shadingUI("forecast_plot_shading_DDD")
                            )
                 ),
                 navbarMenu("Tabeller", icon = icon("globe"),
                            tabPanel("Metadata",
                                     table_modUI("metadata_table")
                            ),
                            tabPanel("Flomtabell",
                                     table_modUI("RL_table")
                            ),
                            tabPanel("HBV med usikkerhetsmodell",
                                     table_modUI("HBV_2014_table")
                            ),
                            tabPanel("HBV med +/- 50% nedbør",
                                     table_modUI("HBV_2016_table")
                            ),
                            tabPanel("DDD",
                                     table_modUI("DDD_table")
                            )
                 ),
                 # navbarMenu("Historikk", icon = icon("history"),
                 #            tabPanel("Kalibreringsresultater"),
                 #            tabPanel("Årsstatistikken: Temperatur"),
                 #            tabPanel("Årsstatistikken: Nedbør"),
                 #            tabPanel("Vannføring: Nedbør")
                 # ),
                 navbarMenu("Dokumentasjon", icon = icon("question"),
                            tabPanel(title = HTML("<a href=\"http://nve.github.io/Flood_forecasting/app.html#how_to_use_it\">Hvordan man bruker app?</a>")),
                            tabPanel(title = HTML("<a href=\"http://NVE.github.io/Flood_forecasting\">Hvordan ble det programmert?</a>")),
                            tabPanel(title = HTML("<a href=\"http://nve.github.io/Flood_forecasting/process.html\">Om modeller</a>"))
                 )
)