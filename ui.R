# This is the user-interface definition of a Shiny web application.

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      
    
      printoutModuleUI("map2_msg"),
      
      mapModuleUI("map2")
      
    ),
    mainPanel(

      forecast_plot_modUI("forecast_plot"),
      
      printoutModuleUI("map1_msg"),
      mapModuleUI("map1")
  
    )
  )
)