# This is the server logic for a Shiny web application.


source('global.R')



server <- function(input, output) {
  
  
#   datafile <- callModule(csvFile, "datafile",
#                          stringsAsFactors = FALSE)
#   output$table <- renderDataTable({
#     datafile()
#   })
  
  
  callModule(mapModule,"map1")
  map_input <- callModule(mapModule_polygonFeature,"map2")
  callModule(printoutModule,"map2_msg", map_input)
  
    
}
