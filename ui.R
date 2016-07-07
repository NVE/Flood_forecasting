# This is the user-interface definition of a Shiny web application.

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      
      
      # csvFileInput("datafile", "User data (.csv format)"),
      
      printoutModuleUI("map2_msg"),
      
      mapModuleUI("map2")
      
    ),
    mainPanel(
      mapModuleUI("map1")
      
      
      # dataTableOutput("table")  
    )
  )
)