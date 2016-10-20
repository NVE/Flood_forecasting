

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

# Define UI for application that plots 
shinyUI(fluidPage(theme ="simplex.min.css", 
                  tags$style(type="text/css", 
                             "label {font-size: 12px;}", 
                             ".recalculating {opacity: 1.0;}" 
                  ), 
                  
                  
                  # Application title 
                  tags$h2("Hydrological modelling with different model types"), 
                  p("Model comparisons of HBV, DDD and DDM"), 
                  
                  # mainPanel(
                     tabsetPanel(
                      tabPanel("EntirePlot",dygraphOutput("mydygraph",height = 650)),
                      tabPanel("Seasonal Plot Temp",plotOutput('annualTemp')),
                      tabPanel("Seasonal Plot Rain",plotOutput('annualRainfall')),
                      tabPanel("Seasonal Plot Runoff",plotOutput('annualFlow' ))
                    ),
                 # ),
                  
                  #dygraphOutput("mydygraph",height = 650),
                  #textOutput("message", container = h3),
                  
                  hr(), 
                  
                  fluidRow( 
                    
                    #tags$h3("Model, Catchment, Zoom to period, and forecast period selection:")
                  ), 
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
                                        DT::dataTableOutput("mytable2")),
                               tabPanel('Plot - 1 year',
                                        plotOutput('', height = "600px"))
                               
                             ))
                    ) 
                  )
))


