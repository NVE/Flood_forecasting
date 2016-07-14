# This is the user-interface definition of a Shiny web application.

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      
    mapModuleUI("map1")
      
    ),
    mainPanel(

      forecast_plot_modUI("forecast_plot")
    
    )
  )
)