# This file contains all the plot modules developed for the Flomvarsling shiny app

#' Shiny server module for the single model and single station plot. Used in server.R
#' @param input 
#' @param output 
#' @param session 
#' @param map_input 
#' @param dat 
#' @import ggplot2
#' @importFrom plotly renderPlotly
#' @importFrom dplyr filter
#' @return
#' @export
#'
#' @examples In server.R
#' callModule(forecast_plot_mod,"forecast_plot_HBV_2014", input4plot_HBV_2014, HBV_2014)
forecast_plot_mod <- function(input, output, session, map_input, dat) {
  
  subset2plot <- reactive(dplyr::filter(dat, nbname == map_input$station))
  subset_OBS <- reactive(dplyr::filter(OBS, nbname == map_input$station))
  
  output$plot <- renderPlotly(forecast_plot(subset_OBS(), subset2plot())
  )
  
}

#' Shiny UI module for the single model and single station plot. Used in server.R
#' @param id 
#'
#' @return
#' @export
#'
#' @examples In UI.R
#' tabPanel("HBV med +/- 50% nedbør",
#' mapModuleUI("map_HBV_2016"),
#' singlemodel_forecast_plot_modUI("forecast_plot_HBV_2016")),
singlemodel_forecast_plot_modUI <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  fluidRow(uiOutput(ns("print_msg")),
           plotlyOutput(ns("plot"), height = "800px")
  )
}

#' Shiny UI module for the multimodel and multistation plot. Used in UI.R and in "mapModule_polygonFeatureUI"
#' @param id 
#'
#' @return
#' @export
#'
#' @examples In UI.R
#' tabPanel("Velg stasjoner med nedtrekksmeny", 
#' mapModuleUI("multistation_map", multiple = TRUE),
#' multimod_forecast_selection_modUI("multistation_plot"), forecast_plot_modUI("multistation_plot"))
forecast_plot_modUI <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  fluidRow(uiOutput(ns("print_msg")),
           uiOutput(ns("rendered_plot"), width = "100%"),  # Plot height increases auto with more stations
           plotlyOutput(ns("plot_input"), height = "400px", width = "100%")
  )
}




#' Shiny server module for the multimodel and multistation plot. Used in server.R and in "mapModule_polygonFeature"
#' @param input 
#' @param output 
#' @param session 
#' @param map_input 
#' @param model_1 
#' @param model_2 
#' @param model_3 
#' @param model_4 
#' @param return_levels 
#'
#' @return
#' @export
#'
#' @examples In server.R
#'  input4multi_forecast_plot <- callModule(mapModule,"multistation_map")
#'  callModule(multimod_forecast_plot_mod,"multistation_plot", input4multi_forecast_plot, OBS, HBV_2014, HBV_2016, DDD, HBV_past_year, flomtabell)
multimod_forecast_plot_mod <- function(input, output, session, map_input, OBS, model_1, model_2, model_3, model_4, return_levels = NULL) {
  
  ns <- session$ns

  observe({
    if ("Runoff" %in% input$type_choice) {

      subset2plot_OBS <- eventReactive({map_input$station},
          subset2plot_OBS <- dplyr::filter(OBS, nbname %in% map_input$station & Variable == "Obs") 
        )
            
      
      if (!is.null(model_1)) {
        output$model1_selection <- renderUI({
          selectInput(ns("variable_1"), label = "HBV med usikkerhetmodell", selected  = c("HBV.UM.sim", "HBV.UM.korr", "Lo50", "Hi50"),
                      choices = unique(filter(model_1, Type == "Runoff")$Variable), multiple = TRUE) 
        })
      }
      
      subset2plot_m1 <- eventReactive({ input$variable_1
        map_input$station},
        if (is.null(input$variable_1)) {
          subset2plot_m1 <- NULL
        } else {
          subset2plot_m1 <- dplyr::filter(model_1, nbname %in% map_input$station & Type == "Runoff" & Variable %in% input$variable_1) 
        })
      
      if (!is.null(model_2)) {
        output$model2_selection <- renderUI({
          selectInput(ns("variable_2"), label = "HBV med +/- 50% nedboer", selected  = c("HBV.P.sim", "HBV.P.korr", "P.p50"),
                      choices = unique(filter(model_2, Type == "Runoff")$Variable), multiple = TRUE) 
        })
      }
      
      subset2plot_m2 <- eventReactive({ input$variable_2
        map_input$station},
        if (is.null(input$variable_2)) {
          subset2plot_m2 <- NULL
        } else {
          subset2plot_m2 <- dplyr::filter(model_2, nbname %in% map_input$station & Type == "Runoff" & Variable %in% input$variable_2) 
        })
    
      if (!is.null(model_3)) {
        output$model3_selection <- renderUI({
          selectInput(ns("variable_3"), label = "DDD", selected = "DDD.sim",
                      choices = "DDD.sim", multiple = TRUE)
        })
      }
      
      ## HACK FLO: This way a quick way to integrate the ODM model. Needs more flexibility
      # if (!is.null(model_3)) {
      #   output$model3_selection <- renderUI({
      #     selectInput(ns("variable_3"), label = "ODM", selected = "ODM.sim", 
      #                 choices = "ODM.sim", multiple = TRUE) 
      #   })
      # }
      
      subset2plot_m3 <- eventReactive({ input$variable_3
        map_input$station},
        if (is.null(input$variable_3)) {
          subset2plot_m3 <- NULL
        } else {
          subset2plot_m3 <- dplyr::filter(model_3, nbname %in% map_input$station & Type == "Runoff" & Variable %in% input$variable_3) 
        })
      
      if (!is.null(model_4)) {
        output$model4_selection <- renderUI({
          selectInput(ns("variable_4"), label = "Simuleringer siste ar", 
                      choices = unique(filter(model_4, Type == "Runoff")$Variable), multiple = TRUE) 
        })
      }
      
      subset2plot_m4 <- eventReactive({ input$variable_4
        map_input$station},
        if (is.null(input$variable_4)) {
          subset2plot_m4 <- NULL
        } else {
          subset2plot_m4 <- dplyr::filter(model_4, nbname %in% map_input$station & Type == "Runoff" & Variable %in% input$variable_4)
        })
      
      if (!is.null(return_levels)) {
        output$return_levels <- renderUI({
          selectInput(ns("type_rl"), label = "Gjentaksintervallgrunnlag", 
                      choices = c("Obs", "Sim"), multiple = TRUE) 
        })
      }
      
      subset2plot_rl <- eventReactive({ input$type_rl
        map_input$station},
        if (is.null(input$type_rl)) {
          subset2plot_rl <- NULL
        } else {
          subset2plot_rl <- dplyr::filter(return_levels, nbname %in% map_input$station & Type %in% input$type_rl) 
        })
    } else if ("Input" %in% input$type_choice) {
          
          output$model1_selection <- renderUI({
          selectInput(ns("model1_selection"), label = "Input variabler", selected  = c("Precip", "Temp"),
                      choices = c("Precip", "Temp"), multiple = TRUE) 
        })
      
      output$model2_selection <- renderUI({
        selectInput(ns("model2_selection"), label = "", selected  = "-",
                    choices = "-") 
      })
      output$model3_selection <- renderUI({
        selectInput(ns("model3_selection"), label = "", selected  = "-",
                    choices = "-") 
      })
      output$model4_selection <- renderUI({
        selectInput(ns("model4_selection"), label = "", selected  = "-",
                    choices = "-") 
      })
      output$return_levels <- renderUI({
        selectInput(ns("return_levels"), label = "", selected  = "-",
                    choices = "-") 
      })
      
      subset2plot_m1 <- eventReactive({ input$model1_selection
        map_input$station},
        if (is.null(input$model1_selection)) {
          subset2plot_m1 <- NULL
        } else {
          subset2plot_m1 <- dplyr::filter(model_1, nbname %in% map_input$station & Type == "Input" & Variable %in% input$model1_selection) 
        })
    
      subset2plot_OBS <- reactive(NULL)
      subset2plot_m2 <- reactive(NULL)
      subset2plot_m3 <- reactive(NULL)
      subset2plot_m4 <- reactive(NULL)
      subset2plot_rl <- reactive(NULL)
    
    } else {
    
        output$model1_selection <- renderUI({
        selectInput(ns("model1_selection"), label = "Tilstandsvariabler HBV_UM", selected  = "HBV.UM.Snow",
                    choices = "HBV.UM.Snow", multiple = TRUE) 
      })

      output$model2_selection <- renderUI({
        selectInput(ns("model2_selection"), label = "Tilstandsvariabler HBV_P", selected  = "HBV.P.Snow",
                    choices = "HBV.P.Snow", multiple = TRUE) 
      })
      output$model3_selection <- renderUI({
        selectInput(ns("model3_selection"), label = "Tilstandsvariabler DDD", selected  = c("DDD.Snow", "DDD.GW", "DDD.Soil"),
                    choices = c("DDD.Snow", "DDD.GW", "DDD.Soil"), multiple = TRUE) 
      })
      output$model4_selection <- renderUI({
        selectInput(ns("model4_selection"), label = "", selected  = "-",
                    choices = "-") 
      })
      output$return_levels <- renderUI({
        selectInput(ns("return_levels"), label = "", selected  = "-",
                    choices = "-") 
      })
      
      subset2plot_m1 <- eventReactive({ input$model1_selection
        map_input$station},
        if (is.null(input$model1_selection)) {
          subset2plot_m1 <- NULL
        } else {
          subset2plot_m1 <- dplyr::filter(model_1, nbname %in% map_input$station & Type == "State" & Variable %in% input$model1_selection) 
        })
      
      subset2plot_m2 <- eventReactive({ input$model2_selection
        map_input$station},
        if (is.null(input$model2_selection)) {
          subset2plot_m2 <- NULL
        } else {
          subset2plot_m2 <- dplyr::filter(model_2, nbname %in% map_input$station & Type == "State" & Variable %in% input$model2_selection) 
        })
      
      subset2plot_m3 <- eventReactive({ input$model3_selection
        map_input$station},
        if (is.null(input$model1_selection)) {
          subset2plot_m3 <- NULL
        } else {
          subset2plot_m3 <- dplyr::filter(model_3, nbname %in% map_input$station & Type == "State" & Variable %in% input$model3_selection) 
        })
      
      subset2plot_OBS <- reactive(NULL)
      subset2plot_m4 <- reactive(NULL)
      subset2plot_rl <- reactive(NULL)
      
    }
    
    output$plot <- renderPlotly(multimod_forecast_plot(subset2plot_OBS(), subset2plot_m1(), subset2plot_m2(), 
                                                       subset2plot_m3(), subset2plot_m4(), subset2plot_rl()))
    
    # Using renderUI to automatically increase plotting size when more stations are selected
    output$rendered_plot <- renderUI( plotlyOutput(ns("plot"), 
                                                   height = paste(400 * length(map_input$station), "px", sep ="")) ) 
  })
}



#' Shiny server module to do multi-model plots in the polygon tab (module "mapModule_polygonFeature"). 
#' Needs tidying up with previous function
#' @param input 
#' @param output 
#' @param session 
#' @param selected_stations 
#' @param model_1 
#' @param model_2 
#' @param model_3 
#' @param model_4 
#' @param return_levels 
#' @param variable_1 
#' @param variable_2 
#' @param variable_3 
#' @param variable_4 
#' @param type_rl 
#'
#' @return
#' @export
#'
#' @examples In "mapModulePolygonFeature"
#' callModule(poly_multimod_forecast_plot_mod, "multi_station_plot", as.character(selected_regine_main()), HBV_2014, HBV_2016, DDD, HBV_past_year, flomtabell,
#' input$variable_1, input$variable_2, input$variable_3, input$variable_4, input$type_rl)
poly_multimod_forecast_plot_mod <- function(input, output, session, selected_stations = NULL, model_1, model_2, model_3, model_4, 
                                       return_levels = NULL, variable_1, variable_2, variable_3, variable_4, type_rl) {
  
  ns <- session$ns
  
  subset2plot_m1 = NULL
  subset2plot_m2 = NULL
  subset2plot_m3 = NULL
  subset2plot_m4 = NULL
  subset2plot_rl = NULL
  
  if (length(selected_stations) > 0) {
    
    subset2plot_OBS <- dplyr::filter(OBS, regine.main %in% selected_stations & Variable == "Obs") 
    
    
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
  output$plot <- renderPlotly(multimod_forecast_plot(subset2plot_OBS, subset2plot_m1, subset2plot_m2, subset2plot_m3, subset2plot_m4, subset2plot_rl))
  output$rendered_plot <- renderUI( plotlyOutput(ns("plot"), 
                                                 height = paste(400 * length(selected_stations), "px", sep ="")) ) 
}


#' Shiny UI module to allow swapping between input/state and runoff results in the first tab of the app. Used in UI.R
#' @param id 
#'
#' @return
#' @export
#'
#' @examples In UI.R
#' tabPanel("Velg stasjoner med nedtrekksmeny", 
#' mapModuleUI("multistation_map", multiple = TRUE),
#' multimod_forecast_selection_modUI("multistation_plot"),
#' forecast_plot_modUI("multistation_plot")),
multimod_forecast_selection_modUI <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  fluidRow(
    column(2, radioButtons(ns("type_choice"), label = "Hvilken type variabel vil du plotte?", selected = "Runoff",
                          choices = c("Input", "Runoff", "State")) ),
    # column(2, uiOutput(ns("OBS_choice"))),
    column(2, uiOutput(ns("model1_selection"))),
    column(2, uiOutput(ns("model2_selection"))),
    column(2, uiOutput(ns("model3_selection"))),
    column(2, uiOutput(ns("model4_selection"))),
    column(2, uiOutput(ns("return_levels")))
  )
}
