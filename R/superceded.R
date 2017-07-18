#' multimod_forecast_plot_EXP
#' @description Shiny server module to do multi-model plots. Needs tidy up with other functions...
#' @param input 
#' @param output 
#' @param session 
#' @param selected_stations 
#' @param model_1 
#' @param model_2 
#' @param model_3 
#' @param model_4 
#'
#' @return
#' @export
#'
#' @examples
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



################ Some old code that could be reused later

## This is to detect which stations were missing a model
observe({
  is_msg <- FALSE
  info_msg <- character()
  if (is.data.frame(subset2plot_m1()) && nrow(subset2plot_m1()) == 0) {
    info_msg <- paste(name_model1)
    is_msg <- TRUE
  }
  if (is.data.frame(subset2plot_m2()) && nrow(subset2plot_m2()) == 0) {
    info_msg <- paste(info_msg, name_model2)
    is_msg <- TRUE
  }
  if (is.data.frame(subset2plot_m3()) && nrow(subset2plot_m3()) == 0) {
    info_msg <- paste(info_msg, name_model3)
    is_msg <- TRUE
  }
  if (is.data.frame(subset2plot_m4()) && nrow(subset2plot_m4()) == 0) {
    info_msg <- paste(info_msg, name_model4)
    is_msg <- TRUE
  }
  if (is.data.frame(subset2plot_rl()) && nrow(subset2plot_rl()) == 0) {
    info_msg <- paste(info_msg, "return levels")
    is_msg <- TRUE
  }
  
  if (is_msg) {
    output$msg <- renderText( paste("The following data are unavailable at this station:", info_msg, sep = " ") )
    
  } else {
    output$msg <- renderText("")
  }
  output$print_msg <- renderUI({
    verbatimTextOutput(ns("msg"))
  })
})



#######################################################################################################################

#' SUPERCEDED_mapModule_polygonFeature
#' @description Shiny server module to map ...
#' @param input 
#' @param output 
#' @param session 
#'
#' @return
#' @export
#'
#' @examples
SUPERCEDED_mapModule_polygonFeature <- function(input, output, session) {
  
  # Get coordinates of the selected polygon
  map_selection <- reactive(input$map_selectbox_features$features[[1]]$geometry$coordinates[[1]])
  # Reactive parameters of the stations inside the polygon
  selected_stations_indices <- reactive(which_station_in_polygon(stations, map_selection()))
  selected_regine_main <-      reactive(stations$regine_main[selected_stations_indices()])
  selected_name <-             reactive(stations$name[selected_stations_indices()])
  selected_long <-             reactive(stations$long[selected_stations_indices()])
  selected_lat <-              reactive(stations$lat[selected_stations_indices()])
  
  # Create map and update the color of the completed polygon to green
  map <- reactive(multiple_station_map(stations, selected_regine_main(),
                                       selected_name(), selected_long(), selected_lat()) %>% 
                    addGeoJSON(input$map_selectbox_features, color="green"))
  
  output$map <- renderLeaflet( map()   ) 
  
  output$print_selection <- renderText({ paste("-", selected_regine_main()) })
  
  return(selected_regine_main)
}


#' SUPERCEDED_mapModule_polygonFeatureUI
#' @description Shiny UI module to be used with "mapModule_polygonFeature" ...
#' @param id 
#'
#' @return
#' @export
#'
#' @examples
SUPERCEDED_mapModule_polygonFeatureUI <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  # fluidPage(
  fluidRow(
    
    column(6, leafletOutput(ns("map")) ),
    column(6,
           wellPanel(h4('Select a group of stations with the map, using the polygon or rectangle tools')),
           #              wellPanel(
           #   selectInput(ns("model"), selected = "HBV_2014", 
           #               label = "Choose a model", choices = c("HBV_2014", "HBV_2016", "DDD"))
           #   ),
           wellPanel(
             h4('Selected stations'),    
             verbatimTextOutput(ns("print_selection"))
           )
    ))
}

