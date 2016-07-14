forecast_plot_mod <- function(input, output, session) {
  
  subset2plot <- reactive(dplyr::filter(HBV_2014_GG, regine_main == input$station))  # input$station
  
  output$plot <- renderPlotly({
    ggplotly(forecast_plot(subset2plot()))
  })
  
}

forecast_plot_modUI <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  fluidRow(
  selectInput(ns("station"), selected = "2.11", 
                label = "Choose a station", choices = unique(HBV_2014_GG$regine_main)),
    
  plotlyOutput(ns("plot"))
  )
  
}