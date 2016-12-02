#' forecast_plot_mod
#' @description Shiny server module to plot ...
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
#' @examples
forecast_plot_mod <- function(input, output, session, map_input, dat) {
  
  subset2plot <- reactive(dplyr::filter(dat, nbname == map_input$station))  # input$station
  subset_OBS <- reactive(dplyr::filter(OBS, nbname == map_input$station))  # input$station
  
  output$plot <- renderPlotly(forecast_plot(subset_OBS(), subset2plot())
  )
  
}

#' singlemodel_forecast_plot_modUI
#' @description Shiny UI module to plot ...
#' @param id 
#'
#' @return
#' @export
#'
#' @examples
singlemodel_forecast_plot_modUI <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  fluidRow(uiOutput(ns("print_msg")),
           plotlyOutput(ns("plot"), height = "800px")
  )
}

#' forecast_plot_modUI
#' @description Shiny UI module to plot ...
#' @param id 
#'
#' @return
#' @export
#'
#' @examples
forecast_plot_modUI <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  fluidRow(uiOutput(ns("print_msg")),
           uiOutput(ns("rendered_plot"), width = "100%"),  # Plot height increases auto with more stations
           plotlyOutput(ns("plot_input"), height = "400px", width = "100%")
  )
}


#' forecast_plot_mod_shading
#' @description Shiny server module to plot ...
#' Same plot as forecast_plot_mod but without plotly to get the shading for the current day
#' @param id 
#'
#' @return
#' @export
#'
#' @examples
forecast_plot_mod_shading <- function(input, output, session, map_input, dat) {
  
  subset2plot <- reactive(dplyr::filter(dat, nbname == map_input$station))  # input$station
  
  output$plot <- renderPlot(forecast_plot_shading(subset2plot())
  )
  
}

#' forecast_plot_mod_shadingUI
#' @description Shiny UI module to be used with "forecast_plot_mod_shading"
#' @param id 
#'
#' @return
#' @export
#'
#' @examples
forecast_plot_mod_shadingUI <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  fluidRow(plotOutput(ns("plot"), height = "800px")
  )
}

#' multimod_forecast_plot_mod
#' @description Shiny server module to do multi-model plots. Needs new parametrization!
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
#' @examples
multimod_forecast_plot_mod <- function(input, output, session, map_input, OBS, model_1, model_2, model_3, model_4, return_levels = NULL) {
  
  ns <- session$ns
  # To get the name in char of the data sent to the module
  name_model1 <- as.character(substitute(model_1))
  name_model2 <- as.character(substitute(model_2))
  name_model3 <- as.character(substitute(model_3))
  name_model4 <- as.character(substitute(model_4))
  
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
    } 
  
   
    
     else if ("Input" %in% input$type_choice) {
      
      # if (!is.null(model_1)) {
        output$model1_selection <- renderUI({
          selectInput(ns("model1_selection"), label = "Input variabler", selected  = c("Precip", "Temp"),
                      choices = c("Precip", "Temp"), multiple = TRUE) 
        })
      # }
      
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
    
    
    # else if ("State" %in% input$type_choice) {
      
      # if (!is.null(model_1)) {
      output$model1_selection <- renderUI({
        selectInput(ns("model1_selection"), label = "Tilstandsvariabler HBV_UM", selected  = "HBV.UM.Snow",
                    choices = "HBV.UM.Snow", multiple = TRUE) 
      })
      # }
      
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



#' poly_multimod_forecast_plot_mod
#' @description Shiny server module to do multi-model plots. Needs tidy up with previous function
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
#' @examples
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



#' multimod_forecast_plot_modUI
#' @description Shiny UI module to be used with multimod_forecast_plot
#' @param id 
#'
#' @return
#' @export
#'
#' @examples
multimod_forecast_plot_modUI <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  fluidRow(plotlyOutput(ns("plot"), height = "800px"
  ))
}



#' multimod_forecast_selection_modUI
#' @description Shiny UI module...
#' @param id 
#'
#' @return
#' @export
#'
#' @examples
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


############### FROM BYMAN


#' taylor_mod
#' @description This was an attempt to modularize Bymans functions. Server module. May not work...
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
taylor_mod <- function(input, output, session, selected_stations, model_1, model_2, model_3, model_4 = NULL) {
  
  output$TDplot <- renderPlot({ 
    mydat0<-subset(alldat, Catchment %in% input$catch)
    # now add the model
    taylor.diagram(mydat0[,3], mydat0[,4], pos.cor = TRUE, main = paste("Taylor Diagram for ", input$catch, sep = ""), 
                   ngamma = 6, sd.arcs = 3, ref.sd = TRUE, grad.corr.lines = c(0.2, 0.4, 0.6, 0.8, 0.9, 0.95, 0.99))
    
    taylor.diagram(mydat0[,3], mydat0[,5],, add = TRUE, col = "grey", 
                   pos.cor = TRUE, ngamma = 6, sd.arcs = 3, ref.sd = TRUE, grad.corr.lines = c(0.2, 0.4, 0.6, 0.8, 0.9, 0.95, 0.99))
    taylor.diagram(mydat0[,3], mydat0[,6],, add = TRUE, col = "blue", 
                   pos.cor = TRUE, ngamma = 6, sd.arcs = 3, ref.sd = TRUE, grad.corr.lines = c(0.2, 0.4, 0.6, 0.8, 0.9, 0.95, 0.99))
    taylor.diagram(mydat0[,3], mydat0[,7],, add = TRUE, col = "brown", 
                   pos.cor = TRUE, ngamma = 6, sd.arcs = 3, ref.sd = TRUE, grad.corr.lines = c(0.2, 0.4, 0.6, 0.8, 0.9, 0.95, 0.99))
    taylor.diagram(mydat0[,3], mydat0[,8],, add = TRUE, col = "green", 
                   pos.cor = TRUE, ngamma = 6, sd.arcs = 3, ref.sd = TRUE, grad.corr.lines = c(0.2, 0.4, 0.6, 0.8, 0.9, 0.95, 0.99))
    
    lpos<-1.4*sd(mydat0$Observed)
    legend(lpos,lpos,legend=c("HBV", "NNET", "SVM","GBM","M5","M5c"),pch=19,col=c("red","grey","blue","brown","green","pink"))  
  }) 
  
}

#' taylor_modUI
#' @description This was an attempt to modularize Bymans functions. UI module to be used with "taylor_mod". May not work...
#' @param id 
#'
#' @return
#' @export
#'
#' @examples
taylor_modUI <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  fluidRow(plotOutput(ns("plot_input"), height = "400px", width = "100%")
  )
}


#' dygraph_mod
#' @description This was an attempt to modularize Bymans functions. Server module. May not work...
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
dygraph_mod <- function(input, output, session, selected_stations, model_1, model_2, model_3, model_4 = NULL) {
  
  output$mydygraph <- renderDygraph({
    
    alldat$myDate <- as.Date(alldat$myDate)
    
    dat1<-subset(alldat, Catchment %in% input$catch)
    dat_cropped<-dat1[,-2]
    dat2<-dat_cropped[complete.cases(dat_cropped),]
    dat.z<-zoo(dat2[,2:8],dat2$myDate)
    myts<-as.ts(dat.z)
    dygraph(
      myts#%>%dyRangeSelector()
    )
  })
}

#' dygraph_mod2
#' @description This was an attempt to modularize Bymans functions. Server module. May not work...
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
dygraph_mod2 <- function(input, output, session, selected_stations, model_1, model_2, model_3, model_4 = NULL) {
  
  output$mydygraph2 <- renderDygraph({
    # start dygraph with all the states
    dat3<-subset(alldat, Catchment %in% input$catch &  myDate > as.character(input$dateRange[1]) & myDate <as.character(input$dateRange[2]))
    dat_cropped<-dat3[,-2]
    #dat4<-dat3[complete.cases(dat3),]
    dat.z1<-zoo(dat_cropped[,2:8],dat_cropped$myDate)
    myts1<-as.ts(dat.z1)
    dygraph(
      myts1#%>%dyRangeSelector()
    )
  })
}

#' dygraph_modUI
#' @description This was an attempt to modularize Bymans functions. UI module to be used with "dygraph_mod". May not work...
#' @param id 
#'
#' @return
#' @export
#'
#' @examples
dygraph_modUI <- function(id) {
# Create a namespace function using the provided id
  ns <- NS(id)
  
  fluidRow(dygraphOutput("mydygraph",height = 600)
  )
}

#' mydygraphModule
#' @description This was an attempt to modularize Bymans functions. Server module. May not work...
#' @param input 
#' @param output 
#' @param session 
#'
#' @return
#' @export
#'
#' @examples
mydygraphModule <- function(input, output, session) {
  
  output$module_graph <- renderDygraph({
    
    alldat$myDate <- as.Date(alldat$myDate)
    
    dat_module<-subset(alldat, Catchment %in% input$catchment)
    
    
    dat_cropped<-dat_module[,-2]
    dat2<-dat_cropped[complete.cases(dat_cropped),]
    dat.z1<-zoo(dat2[,2:8],dat2$myDate)
    
    myts<-as.ts(dat.z1)
    
    dygraph(
      myts#%>%dyRangeSelector()
    )
  })
}


#' mydygraphModuleUI
#' @description This was an attempt to modularize Bymans functions. UI module to be used with "mydygraphModule". May not work...
#' @param id 
#'
#' @return
#' @export
#'
#' @examples
mydygraphModuleUI <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  fluidRow(
    selectInput(ns("catchment"), 'Catchment to analyse', as.character(unique(alldat$Catchment))),
    dygraphOutput(ns("module_graph"),height = 600)
  )
}