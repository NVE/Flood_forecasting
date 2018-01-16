## Those functions were authored by Byman, for the historical tab.
## This need better integration with the rest of the app

#' This was an attempt to modularize Bymans functions. Server module. May not work...
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

#' This was an attempt to modularize Bymans functions. UI module to be used with "taylor_mod". May not work...
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


#' This was an attempt to modularize Bymans functions. Server module. May not work...
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

#' This was an attempt to modularize Bymans functions. Server module. May not work...
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

#' This was an attempt to modularize Bymans functions. UI module to be used with "dygraph_mod". May not work...
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

#' This was an attempt to modularize Bymans functions. Server module. May not work...
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


#' This was an attempt to modularize Bymans functions. UI module to be used with "mydygraphModule". May not work...
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


