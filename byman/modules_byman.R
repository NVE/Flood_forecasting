
mydygraphModule <- function(input, output, session) {
  
  output$big_plot <- renderDygraph({
    
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
  
  fluidPage(
  fluidRow(
    selectInput(ns("catchment"), 'Catchment to analyse', as.character(unique(alldat$Catchment))),
    dygraphOutput(ns("big_plot"), height = 600)
  ),
  
  fluidRow(
  textOutput("message", container = h3),
  
  hr(), 
  tags$h3("Model, Catchment, Zoom to period, and forecast period selection:")
  ),
  
#   fluidRow( 
#     
#     renderInput("a")
#   ), 
  
  
  fluidRow( 
                        column(5, 
                               
                               dygraphOutput("zoomed_graph", height = 600)
                               
                        ), 
    column(3, 
           plotOutput("TD_plot", height = "600px")
           
    ) 
  
  ) 
  )
}


mydygraph2Module <- function(input, output, session) {
  
  output$graph <- renderDygraph({
    # start dygraph with all the states
    dat3 <- subset(alldat, Catchment %in% input$catch &  myDate > as.character(input$dateRange[1]) & myDate < as.character(input$dateRange[2]))

    dat_cropped <- dat3[ , -2]
    #dat4<-dat3[complete.cases(dat3),]
    dat.z1 <- zoo(dat_cropped[ , 2:8], dat_cropped$myDate)
    myts1 <- as.ts(dat.z1)
    dygraph(
      myts1 %>% dyRangeSelector()
    )
  })
}


mydygraph2ModuleUI <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  fluidRow(
  dygraphOutput("graph", height = 600)
  )
  
}

TD_module <- function(input, output, session) {

output$TD_plot <- renderPlot({ 
  mydat0 <- subset(alldat, Catchment %in% input$catch)
  # now add the model
  taylor.diagram(mydat0[ , 3], mydat0[ , 4], pos.cor = TRUE, main = paste("Taylor Diagram for ", input$catch, sep = ""), 
                 ngamma = 6, sd.arcs = 3, ref.sd = TRUE, grad.corr.lines = c(0.2, 0.4, 0.6, 0.8, 0.9, 0.95, 0.99))
  
  taylor.diagram(mydat0[ , 3], mydat0[ , 5], add = TRUE, col = "grey", 
                 pos.cor = TRUE, ngamma = 6, sd.arcs = 3, ref.sd = TRUE, grad.corr.lines = c(0.2, 0.4, 0.6, 0.8, 0.9, 0.95, 0.99))
  taylor.diagram(mydat0[ , 3], mydat0[,6], add = TRUE, col = "blue", 
                 pos.cor = TRUE, ngamma = 6, sd.arcs = 3, ref.sd = TRUE, grad.corr.lines = c(0.2, 0.4, 0.6, 0.8, 0.9, 0.95, 0.99))
  taylor.diagram(mydat0[ , 3], mydat0[,7], add = TRUE, col = "brown", 
                 pos.cor = TRUE, ngamma = 6, sd.arcs = 3, ref.sd = TRUE, grad.corr.lines = c(0.2, 0.4, 0.6, 0.8, 0.9, 0.95, 0.99))
  taylor.diagram(mydat0[ , 3], mydat0[,8], add = TRUE, col = "green", 
                 pos.cor = TRUE, ngamma = 6, sd.arcs = 3, ref.sd = TRUE, grad.corr.lines = c(0.2, 0.4, 0.6, 0.8, 0.9, 0.95, 0.99))
  
  lpos<-1.4*sd(mydat0$Observed)
  legend(lpos,lpos,legend=c("HBV", "NNET", "SVM","GBM","M5","M5c"),pch=19,col=c("red","grey","blue","brown","green","pink"))  
})
}

TD_moduleUI <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  plotOutput('plot', height = "600px")
  
  
}



