forecast_plot_mod <- function(input, output, session, map_input, dat) {
  
  subset2plot <- reactive(dplyr::filter(dat, regine.main == map_input$station))  # input$station
  
  output$plot <- renderPlotly(forecast_plot(subset2plot())
                              )
  
}


multimod_forecast_plot_mod <- function(input, output, session, map_input, model_1, model_2, model_3) {
  
  subset2plot_m1 <- NULL
  subset2plot_m2 <- NULL
  subset2plot_m3 <- NULL
  
#   observeEvent(input$model, {
#      if ("HBV_2014" %in% input$model) {
#         subset2plot_m1 <- dplyr::filter(model_1, regine.main == map_input$station & Type == "Runoff") 
#      }
#      if ("HBV_2016" %in% input$model) {
#         subset2plot_m2 <- dplyr::filter(model_2, regine.main == map_input$station & Type == "Runoff") 
#      }
#      if ("DDD" %in% input$model) {
#         subset2plot_m3 <- dplyr::filter(model_3, regine.main == map_input$station & Type == "Runoff") 
#     }
#     output$plot <- renderPlotly(multimod_forecast_plot(subset2plot_m1, subset2plot_m2, subset2plot_m3))
#   })
  
  observeEvent({input$model
                input$variable}, {
    if ("HBV_2014" %in% input$model) {
      subset2plot_m1 <- dplyr::filter(model_1, regine.main == map_input$station & Type == "Runoff" & Variable %in% input$variable) 
    }
    if ("HBV_2016" %in% input$model) {
      subset2plot_m2 <- dplyr::filter(model_2, regine.main == map_input$station & Type == "Runoff" & Variable %in% input$variable) 
    }
    if ("DDD" %in% input$model) {
      subset2plot_m3 <- dplyr::filter(model_3, regine.main == map_input$station & Type == "Runoff" & Variable %in% input$variable) 
    }
    output$plot <- renderPlotly(multimod_forecast_plot(subset2plot_m1, subset2plot_m2, subset2plot_m3))
  })
  
  
  
}

multimod_forecast_plot_modUI <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  fluidPage(
  fluidRow(
    column(6, checkboxGroupInput(ns("model"), selected = "HBV_2014", inline = TRUE,
              label = "Choose a model", choices = c("HBV_2014", "HBV_2016", "DDD"))
    ),
    column(6, checkboxGroupInput(ns("variable"), selected = c("Sim", "Obs"), inline = TRUE,
                                  label = "Choose variables to plot", choices = c("Sim", "Obs", "SimRaw", "SimCorr")))
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