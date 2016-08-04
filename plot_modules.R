forecast_plot_mod <- function(input, output, session, map_input, dat) {
  
  subset2plot <- reactive(dplyr::filter(dat, regine.main == map_input$station))  # input$station
  
  output$plot <- renderPlotly(forecast_plot(subset2plot())
                              )
  
}

forecast_plot_modUI <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  fluidRow(plotlyOutput(ns("plot"), height = "800px")
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

multimod_forecast_plot_mod <- function(input, output, session, map_input, model_1, model_2, model_3, model_4, return_levels = NULL) {
  
  # observeEvent(map_input$station, {js$reset()})
  ns <- session$ns
  # To get the name in char of the data sent to the module
  name_model1 <- as.character(substitute(model_1))
  name_model2 <- as.character(substitute(model_2))
  name_model3 <- as.character(substitute(model_3))
  name_model4 <- as.character(substitute(model_4))
  
  if (!is.null(model_1)) {
    output$model1_selection <- renderUI({
      selectInput(ns("variable_1"), label = paste("Variables for", name_model1), 
                  choices = unique(filter(model_1, Type == "Runoff")$Variable), multiple = TRUE) 
    })
  }

  subset2plot_m1 <- eventReactive({ input$variable_1
    map_input$station},
    if (is.null(input$variable_1)) {
      subset2plot_m1 <- NULL
    } else {
      subset2plot_m1 <- dplyr::filter(model_1, regine.main == map_input$station & Type == "Runoff" & Variable %in% input$variable_1) 
    })
  
  if (!is.null(model_2)) {
  output$model2_selection <- renderUI({
    selectInput(ns("variable_2"), label = paste("Variables for", name_model2), 
                choices = unique(filter(model_2, Type == "Runoff")$Variable), multiple = TRUE) 
  })
  }
  
  subset2plot_m2 <- eventReactive({ input$variable_2
    map_input$station},
    if (is.null(input$variable_2)) {
      subset2plot_m2 <- NULL
    } else {
      subset2plot_m2 <- dplyr::filter(model_2, regine.main == map_input$station & Type == "Runoff" & Variable %in% input$variable_2) 
    })
  
  if (!is.null(model_3)) {
  output$model3_selection <- renderUI({
    # Crazy trick: deparse(substitute(model)) to print the name of the variable
    selectInput(ns("variable_3"), label = paste("Variables for", name_model3), 
                choices = unique(filter(model_3, Type == "Runoff")$Variable), multiple = TRUE) 
  })
  }
  
  subset2plot_m3 <- eventReactive({ input$variable_3
    map_input$station},
    if (is.null(input$variable_3)) {
      subset2plot_m3 <- NULL
    } else {
      subset2plot_m3 <- dplyr::filter(model_3, regine.main == map_input$station & Type == "Runoff" & Variable %in% input$variable_3) 
    })
  
  if (!is.null(model_4)) {
    output$model4_selection <- renderUI({
      # Crazy trick: deparse(substitute(model)) to print the name of the variable
      selectInput(ns("variable_4"), label = paste("Variables for", name_model4), 
                  choices = unique(filter(model_4, Type == "Runoff")$Variable), multiple = TRUE) 
    })
  }
  
  subset2plot_m4 <- eventReactive({ input$variable_4
    map_input$station},
    if (is.null(input$variable_4)) {
      subset2plot_m4 <- NULL
    } else {
      subset2plot_m4 <- dplyr::filter(model_4, regine.main == map_input$station & Type == "Runoff" & Variable %in% input$variable_4) 
    })
  
  if (!is.null(return_levels)) {
    output$return_levels <- renderUI({
      # Crazy trick: deparse(substitute(model)) to print the name of the variable
      selectInput(ns("type_rl"), label = "Choose a method for return periods", 
                  choices = unique(filter(return_levels)$Type), multiple = TRUE) 
    })
  }
  
  subset2plot_rl <- eventReactive({ input$type_rl
    map_input$station},
    if (is.null(input$type_rl)) {
      subset2plot_rl <- NULL
    } else {
      subset2plot_rl <- dplyr::filter(return_levels, regine.main == map_input$station & Type %in% input$type_rl) 
    })
  
#   observe({
#     if (is.null(input$variable_1) &
#         is.null(input$variable_2) &
#                 is.null(input$variable_3) &
#                         is.null(input$variable_4) &
#                                 is.null(input$type_rl)) {
#       output$plot <- renderPlotly(ggplotly(ggplot()))
#                                   
#                                 } else {
  output$plot <- renderPlotly(multimod_forecast_plot(subset2plot_m1(), subset2plot_m2(), 
                                                     subset2plot_m3(), subset2plot_m4(), subset2plot_rl()))
#                                 }
#   })

}

OLD_multimod_forecast_plot <- function(input, output, session, selected_stations = NULL, model_1, model_2, model_3, model_4, 
                                       return_levels = NULL, variable_1, variable_2, variable_3, variable_4, type_rl) {
  
  subset2plot_m1 = NULL
  subset2plot_m2 = NULL
  subset2plot_m3 = NULL
  subset2plot_m4 = NULL
  subset2plot_rl = NULL
  
  if (length(selected_stations) > 0) {
  if (!is.null(variable_1)) {
    subset2plot_m1 <- dplyr::filter(model_1, regine.main %in% selected_stations & Type == "Runoff" & Variable %in% variable_1) 
  }
  
  if (!is.null(variable_2)) {
    subset2plot_m2 <- dplyr::filter(model_2, regine.main %in% selected_stations & Type == "Runoff" & Variable %in% variable_2) 
  }
  
  if (!is.null(variable_3)) {
    subset2plot_m3 <- dplyr::filter(model_3, regine.main %in% selected_stations & Type == "Runoff" & Variable %in% variable_3) 
  }
  
  if (!is.null(variable_4)) {
    subset2plot_m4 <- dplyr::filter(model_4, regine.main %in% selected_stations & Type == "Runoff" & Variable %in% variable_4) 
  }
  
  if (!is.null(return_levels)) {
    subset2plot_rl <- dplyr::filter(return_levels, regine.main %in% selected_stations & Type %in% type_rl) 
  }
  }
  output$plot <- renderPlotly(multimod_forecast_plot(subset2plot_m1, subset2plot_m2, subset2plot_m3, subset2plot_m4, subset2plot_rl))
}

multimod_forecast_plot_modUI <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  fluidRow(plotlyOutput(ns("plot"), height = "800px"
  ))
}

multimod_forecast_selection_modUI <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  fluidRow(
    column(2, uiOutput(ns("model1_selection"))),
    column(2, uiOutput(ns("model2_selection"))),
    column(2, uiOutput(ns("model3_selection"))),
    column(2, uiOutput(ns("model4_selection"))),
    column(2, uiOutput(ns("return_levels")))
  )
}

multimod_forecast_plot_EXP <- function(input, output, session, selected_stations, model_1, model_2, model_3, model_4 = NULL) {
  
  # observeEvent(map_input$station, {js$reset()})
  ns <- session$ns

  print(selected_stations)
  
  if (!is.null(model_1)) {
    name_model1 <- as.character(substitute(model_1))
    output$model1_selection <- renderUI({
      selectInput(ns("variable_1"), label = paste("Variables for", name_model1), 
                  choices = unique(filter(model_1, Type == "Runoff")$Variable), multiple = TRUE) 
    })
  }
  
  subset2plot_m1 <- eventReactive(input$variable_1, {
    if (is.null(input$variable_1)) {
      subset2plot_m1 <- NULL
    } else {
      subset2plot_m1 <- dplyr::filter(model_1, regine.main %in% selected_stations & Type == "Runoff" & Variable %in% input$variable_1) 
    }
  })
  
  if (!is.null(model_2)) {
    output$model2_selection <- renderUI({
      name_model2 <- as.character(substitute(model_2))
      selectInput(ns("variable_2"), label = paste("Variables for", name_model2), 
                  choices = unique(filter(model_2, Type == "Runoff")$Variable), multiple = TRUE) 
    })
  }
  
  subset2plot_m2 <- eventReactive(input$variable_2, {
    if (is.null(input$variable_2)) {
      subset2plot_m2 <- NULL
    } else {
      subset2plot_m2 <- dplyr::filter(model_2, regine.main %in% selected_stations & Type == "Runoff" & Variable %in% input$variable_2) 
    }
  })
  
  if (!is.null(model_3)) {
    name_model3 <- as.character(substitute(model_3))
    output$model3_selection <- renderUI({
      # Crazy trick: deparse(substitute(model)) to print the name of the variable
      selectInput(ns("variable_3"), label = paste("Variables for", name_model3), 
                  choices = unique(filter(model_3, Type == "Runoff")$Variable), multiple = TRUE) 
    })
  }
  
  subset2plot_m3 <- eventReactive(input$variable_3, {
    if (is.null(input$variable_3)) {
      subset2plot_m3 <- NULL
    } else {
      subset2plot_m3 <- dplyr::filter(model_3, regine.main %in% selected_stations & Type == "Runoff" & Variable %in% input$variable_3) 
    }
  })
  
  if (!is.null(model_4)) {
    name_model4 <- as.character(substitute(model_4))
    output$model4_selection <- renderUI({
      # Crazy trick: deparse(substitute(model)) to print the name of the variable
      selectInput(ns("variable_4"), label = paste("Variables for", name_model4), 
                  choices = unique(filter(model_4, Type == "Runoff")$Variable), multiple = TRUE) 
    })
  }
  
  subset2plot_m4 <- eventReactive(input$variable_4, {
    if (is.null(input$variable_4)) {
      subset2plot_m4 <- NULL
    } else {
      subset2plot_m4 <- dplyr::filter(model_4, regine.main %in% selected_stations & Type == "Runoff" & Variable %in% input$variable_4) 
    }
  })
  
  output$plot <- renderPlotly(multimod_forecast_plot_EXP(subset2plot_m1(), subset2plot_m2(), subset2plot_m3(), subset2plot_m4()))
  
}
