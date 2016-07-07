# This is the user-interface definition of a Shiny web application.

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      csvFileInput("datafile", "User data (.csv format)"),
      
      # leafletOutput("map1")
      mapModuleUI("test_map2")
      
    ),
    mainPanel(
      mapModuleUI("test_map")
      
      
      # dataTableOutput("table")  
    )
  )
)