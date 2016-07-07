# This is the server logic for a Shiny web application.


source('global.R')



server <- function(input, output) {
  
  
#   datafile <- callModule(csvFile, "datafile",
#                          stringsAsFactors = FALSE)
#   output$table <- renderDataTable({
#     datafile()
#   })
  
  
  map1_input <- callModule(mapModule_polygonFeature,"map1") 
  callModule(printoutModule,"map1_msg", map1_input)
  
  map2_input <- callModule(mapModule_polygonFeature,"map2")
  callModule(printoutModule,"map2_msg", map2_input)
  
    
}
