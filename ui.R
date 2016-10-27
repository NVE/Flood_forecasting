
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


# This is the user-interface definition of a Shiny web application.
ui <- navbarPage("Flomvarsling", collapsible = TRUE, theme = "my_style.css",
                 
                 #                  tabPanel("About", icon = icon("info"),
                 #                           
                 #                           fluidRow(
                 #                             column(6, wellPanel(
                 #                               wellPanel(includeMarkdown("about.md")))
                 # 
                 #                               ),
                 #                             column(6,
                 #                               img(src='flood.jpg', align = "right")
                 #                             )),
                 #                           # TAB 1
                 #                           fluidRow(
                 #                             column(6,
                 #                                    wellPanel(includeMarkdown("tab1.md"))
                 #                                    ),
                 #                             column(6,
                 #                                    img(id = "tab1", src='multimodel_tab_with_warning.png', align = "right")
                 #                           )),
                 #                           bsModal("modalExample", "Your plot", "tab1", size = "large",
                 #                                   img(src='multimodel_tab_with_warning.png', align = "center")),
                 #                           bsTooltip("tab1", "Click to enlarge", 
                 #                                     "left", options = list(container="body")),
                 #                  
                 #                           # TAB 2
                 #                           fluidRow(
                 #                             column(6,
                 #                                    wellPanel(includeMarkdown("tab2.md"))),
                 #                             column(6,
                 #                                    img(id = "tab2", src='multimodel_tab_with_warning.png', align = "right")
                 #                             )),
                 #                           bsModal("modalExample", "Your plot", "tab2", size = "large",
                 #                                   img(src='multimodel_tab_with_warning.png', align = "center")),
                 #                           # TAB 3
                 #                           fluidRow(
                 #                             column(6,
                 #                                    wellPanel(includeMarkdown("tab3.md"))),
                 #                             column(6,
                 #                                    img(id = "tab3", src='multimodel_tab_with_warning.png', align = "right")
                 #                             )),
                 #                           bsModal("modalExample", "Your plot", "tab3", size = "large",
                 #                                   img(src='multimodel_tab_with_warning.png', align = "center"))
                 #           
                 #                  ),
                 
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
                          NEW_mapModule_polygonFeatureUI("map_polygon")
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
                 navbarMenu("Table tools", icon = icon("globe"),
                            tabPanel("Metadata",
                                     table_modUI("metadata_table")
                            ),
                            tabPanel("Return levels",
                                     table_modUI("RL_table")
                            ),
                            tabPanel("HBV_2014",
                                     table_modUI("HBV_2014_table")
                            ),
                            tabPanel("HBV_2016",
                                     table_modUI("HBV_2016_table")
                            ),
                            tabPanel("DDD",
                                     table_modUI("DDD_table")
                            )
                 ),
         
         ##############################
         #-- byman ####################
         
                 navbarMenu("Historical tools", icon = icon("history"),
                            tabPanel("EntirePlot",dygraphOutput("mydygraph",height = 650)),
                            tabPanel("Seasonal Plot Temp",plotOutput('annualTemp')),
                            tabPanel("Seasonal Plot Rain",plotOutput('annualRainfall')),
                            tabPanel("Seasonal Plot Runoff",plotOutput('annualFlow' ))
                 ),
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


         
         
         
)
