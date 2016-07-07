# This is the server logic for a Shiny web application.


source('modules.R')

server <- function(input, output) {
  
  
  datafile <- callModule(csvFile, "datafile",
                         stringsAsFactors = FALSE)
  
  #   output$table <- renderDataTable({
  #     datafile()
  #   })
  
  
  callModule(mapModule2,"test_map")
  callModule(mapModule2,"test_map2")
  
    
}
