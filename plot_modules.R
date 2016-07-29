forecast_plot_mod <- function(input, output, session, map_input, dat) {
  
  subset2plot <- reactive(dplyr::filter(dat, regine.main == map_input$station))  # input$station
  
  output$plot <- renderPlotly(forecast_plot(subset2plot())
                              )
  
}


multimod_forecast_plot_mod <- function(input, output, session, map_input, model_1, model_2, model_3) {
  
  subset2plot_m1 <- eventReactive({ input$variable_1
                                    map_input$station},
                                  if (is.null(input$variable_1)) {
                                    subset2plot_m1 <- NULL
                                  } else {
                                    dplyr::filter(model_1, regine.main == map_input$station & Type == "Runoff" & Variable %in% input$variable_1) 
                                  })
  
  subset2plot_m2 <- eventReactive({ input$variable_2
                                    map_input$station},
                                  if (is.null(input$variable_2)) {
                                    subset2plot_m2 <- NULL
                                  } else {
                                    dplyr::filter(model_2, regine.main == map_input$station & Type == "Runoff" & Variable %in% input$variable_2) 
                                  })
  
  subset2plot_m3 <- eventReactive({ input$variable_3
                                    map_input$station},
                                  if (is.null(input$variable_3)) {
                                    subset2plot_m3 <- NULL
                                  } else {
                                    dplyr::filter(model_3, regine.main == map_input$station & Type == "Runoff" & Variable %in% input$variable_3) 
                                  })
  
  output$plot <- renderPlotly(multimod_forecast_plot(subset2plot_m1(), subset2plot_m2(), subset2plot_m3()))

}

multimod_forecast_plot_modUI <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  fluidPage(
  fluidRow(
    column(3, selectInput(ns("variable_1"), label = "Variables for HBV_2014", choices = unique(filter(HBV_2014, Type == "Runoff")$Variable), multiple = TRUE) ),
    column(3, selectInput(ns("variable_2"), label = "Variables for HBV_2016", choices = unique(filter(HBV_2016, Type == "Runoff")$Variable), multiple = TRUE) ),
    column(3, selectInput(ns("variable_3"), label = "Variables for DDD", choices = unique(filter(DDD, Type == "Runoff")$Variable), multiple = TRUE) )
  ),
  fluidRow(plotlyOutput(ns("plot"), height = "800px")
  )
  )
}




forecast_plot_mod2 <- function(input, output, session, selected_stations, dat) {
  
  print(selected_stations)
  
  subset2plot <- reactive(dplyr::filter(dat, regine.main %in% selected_stations & Type == "Runoff"))  
  
  output$plot <- renderPlotly(forecast_plot2(subset2plot())
  )
}

# Same plot but without plotly to get the shading for the current day
forecast_plot_mod_shading <- function(input, output, session, map_input, dat) {
  
  subset2plot <- reactive(dplyr::filter(dat, regine.main == map_input$station))  # input$station
  
  output$plot <- renderPlot(forecast_plot_shading(subset2plot())
  )
  
}


# Same plot but without plotly to get the shading for the current day
forecast_plot_mod_shading2 <- function(input, output, session, selected_stations, dat) {
  
  subset2plot <- reactive(dplyr::filter(dat, regine.main %in% selected_stations() & Type == "Runoff")) 
  
  output$plot <- renderPlot(forecast_plot_shading(subset2plot()))
  
}

forecast_plot_mod_shadingUI <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  fluidRow(plotOutput(ns("plot"), height = "800px")
  )
}


forecast_plot_modUI <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  fluidRow(plotlyOutput(ns("plot"), height = "800px")
           )
}


TEST_forecast_plot_modUI <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  plotlyOutput(ns("plot"), height = "800px")
  
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