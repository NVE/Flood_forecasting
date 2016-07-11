renderInputs <- function(prefix) {
  wellPanel( 
    fluidRow( 
      column(3, 
             selectInput('catch', 'Catchment to analyse', as.character(unique(alldat$Catchment))),
             
             
             dateRangeInput('dateRange',
                            label = ' Zoom plot: Start Date range input: dd/mm/yy',
                            start = range(mydat$myDate)[1]+90, end = range(mydat$myDate)[1]+190
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
                                c("HBV",  "DDD", "DDM"),
                                selected=c("HBV",  "DDD", "DDM")
             )),
      column(2,
             
             checkboxGroupInput('show_meas','Model fitness performance:',
                                names(allmeas)[3:8],selected = names(allmeas)[3:8])
             #                   helpText('For the efficiency measures data, we can select variables
             #                        to show in the table; for the mtcars example, we
             #                        use orderClasses = TRUE so that sorted columns 
             #                        are colored since they have special CSS classes 
             #                        attached; for the data, we customize the 
             #                        length menu so we can display 5 rows per page.')
      )
      
      ## HACK FLO
      
#       column(2, wellPanel(
#         selectInput(inputId='station', selected =  station$number[37], 
#                     label = "Pick a station", choices = station$number)
#       )
#       ),
#       column(4, leafletOutput('map'))
      
      ## HACK FLO END
      
      ))
  
} 



# Define UI for application that plots 
ui <- fluidPage(theme="simplex.min.css", 
                  tags$style(type="text/css", 
                             "label {font-size: 12px;}", 
                             ".recalculating {opacity: 1.0;}" 
                  ), 
                  
                  
                  # Application title 
                  tags$h2("Hydrological modelling with different model types"), 
                  p("Model comparisons of HBV, DDD and DDM"), 
                  
                  dygraphOutput("mydygraph",height = 600),

                
                
                
                  textOutput("message", container = h3),
                  
                  hr(), 
                  
                  
                  fluidRow( 
                    
                    tags$h3("Model, Catchment, Zoom to period, and forecast period selection:")
                  ), 
                  fluidRow( 
                    
                    renderInputs("a")
                  ), 
                  
                  
                  fluidRow( 
                    column(5, 
                           
                           dygraphOutput("mydygraph2",height = 600)
                           
                    ), 
                    column(3, 
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
                ,
                # fluidRow(
                mydygraphModuleUI("dygraph1"),
                mydygraphModuleUI("dygraph2"),
                mydygraphModuleUI("dygraph3")
                # )
                  
                  
)
