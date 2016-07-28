forecast_plot_mod <- function(input, output, session, map_input, dat) {
  
  subset2plot <- reactive(dplyr::filter(dat, regine.main == map_input$station))  # input$station
  
  output$plot <- renderPlotly(forecast_plot(subset2plot())
                              )
  
}

forecast_plot_mod2 <- function(input, output, session, selected_stations, dat) {
  
  subset2plot <- reactive(dplyr::filter(dat, regine.main == selected_stations[1]))  # input$station
  
  output$plot <- renderPlotly(forecast_plot(subset2plot())
  )
  
}

# Same plot but without plotly to get the shading for the current day
forecast_plot_mod_shading <- function(input, output, session, map_input, dat) {
  
  subset2plot <- reactive(dplyr::filter(dat, regine.main == map_input$station))  # input$station
  
  output$plot <- renderPlot(forecast_plot_shading(subset2plot())
  )
  
}

forecast_plot_mod_shadingUI <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  fluidRow(plotOutput(ns("plot"))
  )
}


forecast_plot_modUI <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  fluidRow(plotlyOutput(ns("plot"))
           )
}


# forecast_plot_modUI <- function(id) {
#   # Create a namespace function using the provided id
#   ns <- NS(id)
#   
#   fluidRow(
#   selectInput(ns("station"), selected = "2.11", 
#                 label = "Choose a station", choices = stations_available),
#     
#   plotlyOutput(ns("plot"))
#   )
# }