forecast_plot_mod <- function(input, output, session, map_input, dat) {
  
  subset2plot <- reactive(dplyr::filter(dat, nbname == map_input$station))  # input$station
  
  output$plot <- renderPlotly(forecast_plot(subset2plot())
  )
  
}

OLD_forecast_plot_modUI <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  fluidRow(uiOutput(ns("print_msg")),
           plotlyOutput(ns("plot"), height = "800px")
           #uiOutput(ns("rendered_plot"), width = "100%")  # Plot height increases auto with more stations
  )
}


forecast_plot_modUI <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  fluidRow(uiOutput(ns("print_msg")),
           uiOutput(ns("rendered_plot"), width = "100%"),
           plotlyOutput(ns("plot_input"), height = "400px", width = "100%")
  )
}

# Same plot but without plotly to get the shading for the current day
forecast_plot_mod_shading <- function(input, output, session, map_input, dat) {
  
  subset2plot <- reactive(dplyr::filter(dat, nbname == map_input$station))  # input$station
  
  output$plot <- renderPlot(forecast_plot_shading(subset2plot())
  )
  
}

forecast_plot_mod_shadingUI <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  fluidRow(plotOutput(ns("plot"), height = "800px")
  )
}

multimod_forecast_plot_mod <- function(input, output, session, map_input, model_1, model_2, model_3, model_4, return_levels = NULL) {
  
  ns <- session$ns
  # To get the name in char of the data sent to the module
  name_model1 <- as.character(substitute(model_1))
  name_model2 <- as.character(substitute(model_2))
  name_model3 <- as.character(substitute(model_3))
  name_model4 <- as.character(substitute(model_4))
  
  observe({
    if ("Runoff" %in% input$type_choice) {
      
      
      if (!is.null(model_1)) {
        output$model1_selection <- renderUI({
          selectInput(ns("variable_1"), label = paste("Variables for", name_model1), selected  = c("SimRaw", "SimCorr", "SimL50", "SimH50"),
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
          selectInput(ns("variable_2"), label = paste("Variables for", name_model2), selected  = c("SimRaw", "SimCorr", "SimP50"),
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
          selectInput(ns("variable_3"), label = paste("Variables for", name_model3), selected = c("DDD.Sim", "Obs"), 
                      choices = unique(filter(model_3, Type == "Runoff")$Variable), multiple = TRUE) 
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
          selectInput(ns("variable_4"), label = paste("Variables for", name_model4), 
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
          selectInput(ns("type_rl"), label = "Choose a method for return periods", 
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
      
      output$plot <- renderPlotly(multimod_forecast_plot(subset2plot_m1(), subset2plot_m2(), 
                                                         subset2plot_m3(), subset2plot_m4(), subset2plot_rl()))
      # Using renderUI to automatically increase plotting size when more stations are selected
      output$rendered_plot <- renderUI( plotlyOutput(ns("plot"), 
                                                     height = paste(400 * length(map_input$station), "px", sep ="")) ) 
      
    } 
    
#     else {
#       
#       subset2plot_i1 <- dplyr::filter(model_1, nbname %in% map_input$station & Type == "Input") 
#       subset2plot_i2 <- dplyr::filter(model_2, nbname %in% map_input$station & Type == "Input") 
#       subset2plot_i3 <- dplyr::filter(model_3, nbname %in% map_input$station & Type == "Input") 
#       
#       output$plot <- renderPlotly(multimod_forecast_plot(subset2plot_i1, subset2plot_i2, 
#                                                          subset2plot_i3))
#       
#       output$rendered_plot <- renderUI( plotlyOutput(ns("plot"), 
#       height = paste(400 * length(map_input$station), "px", sep ="")) ) 
#       
#     }
    
    
  })
  
  
  #### TEST: will have to be in an apply of for loop later
  
  observe({
    if ("Input" %in% input$type_choice) {
      
      subset2plot_i1 <- dplyr::filter(model_1, nbname %in% map_input$station & Type %in% input$type_choice) 
      subset2plot_i2 <- dplyr::filter(model_2, nbname %in% map_input$station & Type %in% input$type_choice) 
      subset2plot_i3 <- dplyr::filter(model_3, nbname %in% map_input$station & Type %in% input$type_choice) 
      output$plot_input <- renderPlotly(multimod_forecast_plot(subset2plot_i1, subset2plot_i2, 
                                                               subset2plot_i3))
     
    }
  })
  
}

OLD_multimod_forecast_plot <- function(input, output, session, selected_stations = NULL, model_1, model_2, model_3, model_4, 
                                       return_levels = NULL, variable_1, variable_2, variable_3, variable_4, type_rl) {
  
  ns <- session$ns
  
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
  output$rendered_plot <- renderUI( plotlyOutput(ns("plot"), 
                                                 height = paste(400 * length(selected_stations), "px", sep ="")) ) 
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
    column(2, selectInput(ns("type_choice"), label = "Choose the type of variable to plot", selected = "Runoff",
                          choices = c("Input", "Runoff", "State"), multiple = TRUE) ),
    column(2, uiOutput(ns("model1_selection"))),
    column(2, uiOutput(ns("model2_selection"))),
    column(2, uiOutput(ns("model3_selection"))),
    column(2, uiOutput(ns("model4_selection"))),
    column(2, uiOutput(ns("return_levels")))
  )
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
    column(2, selectInput(ns("type_choice"), label = "Choose the type of variable to plot", selected = "Runoff",
                          choices = c("Input", "Runoff", "State"), multiple = TRUE) ),
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


############### FROM BYMAN

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

taylor_modUI <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  fluidRow(plotOutput(ns("plot_input"), height = "400px", width = "100%")
  )
}


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

dygraph_modUI <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  fluidRow(dygraphOutput("mydygraph",height = 600)
  )
}

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


mydygraphModuleUI <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  fluidRow(
    selectInput(ns("catchment"), 'Catchment to analyse', as.character(unique(alldat$Catchment))),
    dygraphOutput(ns("module_graph"),height = 600)
  )
}
