# This is the user-interface logic for the Flood forecasting Shiny App.

ui <- navbarPage(title = HTML("<a href=\"http://NVE.github.io/Flood_forecasting\">Flomvarsling</a>"), collapsible = TRUE, theme = "my_style.css",
                 
                 navbarMenu("Multistasjon / Multimodell", icon = icon("random"),
                            
                            tabPanel("Velg stasjoner med nedtrekksmeny", 
                                     mapModuleUI("multistation_map", multiple = TRUE),
                                     multimod_forecast_selection_modUI("multistation_plot"),
                                     forecast_plot_modUI("multistation_plot")
                            ),
                            tabPanel("Velg stasjoner med polygon",
                                     ## First tried to implement this tab the same way as the first, but exchanging the inputs between modules did not work
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
                            ),
                            tabPanel("DDD",
                                     mapModuleUI("map_DDD"),
                                     singlemodel_forecast_plot_modUI("forecast_plot_DDD")
                            ),
                            tabPanel("ODM",
                                     mapModuleUI("map_ODM"),
                                     singlemodel_forecast_plot_modUI("forecast_plot_ODM")
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
                            ),
                            tabPanel("ODM",
                                     table_modUI("ODM_table")
                            )
                 ),
                 navbarMenu("Historikk", icon = icon("history"),
                            tabPanel("Kalibreringsresultater"),
                            tabPanel("Årsstatistikken: Temperatur"),
                            tabPanel("Årsstatistikken: Nedbør"),
                            tabPanel("Vannføring: Nedbør")
                 ),
                 navbarMenu("Dokumentasjon", icon = icon("question"),
                            tabPanel(title = HTML("<a href=\"http://nve.github.io/Flood_forecasting/app.html#how_to_use_it\">Hvordan man bruker app?</a>")),
                            tabPanel(title = HTML("<a href=\"http://NVE.github.io/Flood_forecasting\">Hvordan ble det programmert?</a>")),
                            tabPanel(title = HTML("<a href=\"http://nve.github.io/Flood_forecasting/process.html\">Om modeller</a>")),
                            tabPanel(title = HTML("<a href=\"https://github.com/NVE/Flood_forecasting/issues\">Problem med appen? Skriv om det!</a>"))
                            
                 ),
                 
                 navbarMenu(paste("Last update: ", update_time), icon = icon("refresh")
                            ## We could consider adding a summary of past forecasts under this navbarMenu. Maybe with previously saved rmarkdown reports
                            # ,
                            # tabPanel("Yesterday's report")
                 )
)