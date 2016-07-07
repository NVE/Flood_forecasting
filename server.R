# This is the server logic for a Shiny web application.


source('global.R')



server <- function(input, output) {
  
  
  datafile <- callModule(csvFile, "datafile",
                         stringsAsFactors = FALSE)
  
  #   output$table <- renderDataTable({
  #     datafile()
  #   })
  
  
  callModule(mapModule,"test_map")
  callModule(mapModule_polygonFeature,"test_map2")
  
    
}
