
renderInputs <- function(prefix) {
  
  wellPanel( 
    fluidRow( 
      column(2, 
             selectInput('catch', 'Catchment to analyse', choices=c("",as.character(unique(stnDatMaster$stN))),selected = "2.323_Fura")),
      
      column(1, 
             checkboxGroupInput('show_vars',
                                'Model Selection:',
                                c("HBV",  "DDD", "DDM","ODM"),
                                selected=c("HBV",  "DDD", "DDM","ODM"))),
      
      column(2,
             
             checkboxGroupInput('show_meas','Model fitness performance:',
                                c("model","RMSE","NSE","KGE","BIAS-%"),selected = c("model","RMSE","NSE","KGE","BIAS-%"))
             #names(evTable)[2:6],selected = names(evTable)[2:6])
             #                   helpText('For the efficiency measures data, we can select variables
             #                        to show in the table; for the mtcars example, we
             #                        use orderClasses = TRUE so that sorted columns 
             #                        are colored since they have special CSS classes 
             #                        attached; for the data, we customize the 
             #                        length menu so we can display 5 rows per page.')
      ),
      column(3,
             h3(textOutput('myText'))),
      
      column(2,
             #h3(textOutput('myText'))
             dateRangeInput('dateRange',
                            label = ' Zoom plot: Start Date range input: dd/mm/yy',
                            start = range(range(stnDatMaster$Date))[1], end = range(range(stnDatMaster$Date))[2]),
             
             
             dateRangeInput('dateRange2',
                            label = paste('Forecasting data range: ,',
                                          'dd/mm/yy'),
                            start = Sys.Date() - 1, end = Sys.Date() +10,
                            min = Sys.Date() , max = Sys.Date() + 10,
                            separator = " to ", format = "yyyy-mm-dd"))#,
      
      #             column(2,
      #               h3(textOutput('myText'))
      #                   )
    ))
  
} 


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

         ##############################
         #-- byman ####################
         
                 navbarMenu("Historikk", icon = icon("history"),
                            tabPanel("Kalibreringsresultater", 
                                     
                                     dygraphOutput("mydygraph",height = 650),
                                     
                                     hr(), 
                                     
                                     fluidRow( 
                                       
                                       renderInputs("a")
                                     ), 
                                     
                                     
                                     fluidRow( 
                                       column(4, 
                                              mainPanel(
                                                tabsetPanel(
                                                  tabPanel('Model performnce',
                                                           # dygraphOutput("mygraph2",height = 600)),
                                                           DT::dataTableOutput("mytable")),
                                                  tabPanel('Peaks: Hits/Misses',
                                                           DT::dataTableOutput("tablehits"))
                                                  # tabPanel('Accum. Diff.',
                                                  #          DT::dataTableOutput("mytable11"))
                                                  
                                                ))),
                                       
                                       column(3, 
                                              plotOutput('TDplot', height = "600px")), 
                                       
                                       column(5,
                                              mainPanel(
                                                tabsetPanel(
                                                  tabPanel('Plot - 9 d. ahead',
                                                           # dygraphOutput("mygraph2",height = 600)),
                                                           plotOutput('mygraph3', height = "600px")),
                                                  tabPanel('Table- 9 d. ahead',
                                                           DT::dataTableOutput("mytable1")),
                                                  tabPanel('Plot - 20 d. past ',
                                                           # dygraphOutput("mygraph2",height = 600)),
                                                           plotOutput('mygraph2', height = "600px")),
                                                  
                                                  tabPanel('Month Table',
                                                           DT::dataTableOutput("mytable2"))#,
                                                  # tabPanel('Plot - 1 year',
                                                  #          plotOutput('', height = "600px"))
                                                  
                                                ))
                                       ) 
                                     )
                                     
                                     ),
                            tabPanel("Årsstatistikken: Temperatur", plotOutput('annualTemp')),
                            tabPanel("Årsstatistikken: Nedbør", plotOutput('annualRainfall')),
                            tabPanel("Årsstatistikken: Vannføring", plotOutput('annualFlow' ))
                 ),

           
                 navbarMenu("Dokumentasjon", icon = icon("question"),
                            tabPanel(title = HTML("<a href=\"http://nve.github.io/Flood_forecasting/app.html#how_to_use_it\">Hvordan man bruker app?</a>")),
                            tabPanel(title = HTML("<a href=\"http://NVE.github.io/Flood_forecasting\">Hvordan ble det programmert?</a>")),
                            tabPanel(title = HTML("<a href=\"http://nve.github.io/Flood_forecasting/process.html\">Om modeller</a>"))
                 )
)

