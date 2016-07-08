library(shiny)
library(ggplot2)

renderInputs <- function(prefix) { 
     wellPanel( 
         fluidRow( 
             column(3, 
                    selectInput('catch', 'Catchment to analyse', as.character(unique(alldat$Catchment))),
                    

                  dateRangeInput('dateRange',
                                 label = ' Zoom plot: Start Date range input: dd/mm/yy',
                                 start = range(mydat$myDate)[1], end = range(mydat$myDate)[2]
                  ),
                  
                  dateRangeInput('dateRange2',
                                 label = paste('Forecasting data range: ,',
                                               'dd/mm/yy'),
                                 start = Sys.Date() - 1, end = Sys.Date() +10,
                                 min = Sys.Date() , max = Sys.Date() + 10,
                                 separator = " to ", format = "yyyy-mm-dd"

                  )),
              column(5,
                     h3(textOutput('myText'))
                     
                     
                     ),
              column(2, 
                     checkboxGroupInput('show_vars',
                                        'Model Selection:',
                                        names(alldat)[4:9],
                                        selected = names(alldat)[4:9])
                ),
              column(2,

                     checkboxGroupInput('show_meas','Model fitness performance:',
                  names(allmeas)[3:8],selected = names(allmeas)[3:8])
#                   helpText('For the efficiency measures data, we can select variables
#                        to show in the table; for the mtcars example, we
#                        use orderClasses = TRUE so that sorted columns 
#                        are colored since they have special CSS classes 
#                        attached; for the iris data, we customize the 
#                        length menu so we can display 5 rows per page.')
             ) ))
    
         } 



# Define UI for application that plots 
shinyUI(fluidPage(theme="simplex.min.css", 
                        tags$style(type="text/css", 
                                          "label {font-size: 12px;}", 
                                          ".recalculating {opacity: 1.0;}" 
                                        ), 
                     
                     
                        # Application title 
                        tags$h2("Hydrological modelling with different model types"), 
                        p("Model comparisons of HBV, DDD and DDM"), 
                  #plotOutput('Allplot'),
                  mainPanel(
                    tabsetPanel(
                      tabPanel('Historical simulations',
                               plotOutput('AllPlot', height = "600px")),
                      tabPanel('Forecasting..',
                               dataTableOutput("mytable3"))
                      
                    )),
                     
                  hr(), 
                      
                     
                        fluidRow( 

                          tags$h3("Model, Catchment, Zoom to period, and forecast period selection:")
                          ), 
                        fluidRow( 

                          renderInputs("a")
                          ), 

                     
                       fluidRow( 
                            column(4, 
                                   mainPanel(
                                     tabsetPanel(
                                       tabPanel('Plot',
                                                plotOutput('zoomPlot', height = "600px")),
                                       tabPanel('Table',
                                                dataTableOutput("mytable3"))
                                       
                                     ))),
 #                                           plotOutput('zoomPlot', height = "600px") 
                                         # ), 
                            column(4, 
                                            plotOutput('TDplot', height = "600px") 
                                          ), 
                           
                            column(4,
                                   mainPanel(
                                     tabsetPanel(
                                       tabPanel('Only Selected',
                                                dataTableOutput("mytable2")),
                                       tabPanel('All Others',
                                                dataTableOutput("mytable3"))

                                       ))
                          ) 
                      )
))


# shinyUI(fluidPage(
#   
#   tags$h2("Data Driven Modelling (DDM) comaprisons with HBV "),
#   
#   #plotOutput('plot'),
#   
#   hr(),
#   
#   fluidRow(
# 
#     column(4, 
#            selectInput('x', 'Catchment to analyse', names(hbv.All)),
#            #selectInput('y', 'Machine learning Model', names(hbv.All), names(hbv.All)[[2]]),
#            selectInput('y', 'Model selection', models, 
#                        choices=c("AllModels","HBV","DDD","NeuralNetwork","SupportVectorMachine", "Generlised Boosted Regression","Model Trees","Model Trees -Cubic")),
#            selectInput('color', 'Plot Type', plotTypes, 
#                        choices=c("Ordinary Hydrograph","Taylor Diagram","Performance Indicators" ))
#     ),
#     column(4,
#            dateRangeInput('dateRange',
#                           label = 'Start Date range input: yyyy-mm-dd',
#                           start = Sys.Date() - 2, end = Sys.Date() + 2
#            ),
#            
#            dateRangeInput('dateRange2',
#                           label = paste('End Date range input 2: ,',
#                                         'dd/mm/yy'),
#                           start = Sys.Date() - 3, end = Sys.Date() + 3,
#                           min = Sys.Date() - 10, max = Sys.Date() + 10,
#                           separator = " - ", format = "dd/mm/yy",
#                           startview = 'year', language = 'fr', weekstart = 1
#            )),
#            column(3,
#                   h4(""),
#                   sliderInput('dataRange', 'Data Range for testing', 
#                               min=5845, max=nrow(hbv.All),
#                               value=min(30, nrow(hbv.All)), 
#                               step=50, round=0),
#                   br(),
#                   checkboxInput('jitter', 'Jitter'),
#                   checkboxInput('smooth', 'Smooth')
#            )),
#               # Show the caption, a summary of the performance measures
#           
#             plotOutput('plot',height = "600px"),
#   hr(),
#             
#             fluidRow(
#               
#               column(4,
#                      plotOutput('plot',height = "600px")
#                      ),
#               column(4,
#                      plotOutput('plot')
#                      )
#                      
#                 ) 
# #            mainPanel(
# #              h3(textOutput("caption", container = span)),
# #              
# #              verbatimTextOutput("summary"), 
# #              
# #              tableOutput("view"))
#    
#   
# ))