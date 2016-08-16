# setwd("./byman")
source("./global.R")


server <- function(input, output,session) {
  
  filename=reactive({
    paste0(input$torre,input$tipo)
  })
  day1=reactive({
    as.POSIXct(input$date1)
  })
  day2=reactive({
    as.POSIXct(input$date2)    
  })
  
  var <- reactive({
    input$catch
  })
  
  output$mytable1 = renderTable({
    allmeas
  })
  
  # Return the requested dataset
  datasetInput <- reactive({
    switch(input$allmeas,
           "76.5_Nigardsbrevatn"=Nigardsbrevatn,
           "55.4_Roykenes"=Roykenes,
           "2.604_Elverum"= Elverum,
           "2.32_Atna"=Atna,
           "2.11_Narsjo"= Narsjo)
  })
  output$mytable1 = renderTable({
    allmeas
  })
  
  # choose columns to display
  # sorted columns are colored now because CSS are attached to them
  output$mytable2 = renderDataTable({
    mymeas<-subset(allmeas, Catchment %in% input$catch)
    mymeas<-DT::datatable(mymeas[ ,c("Model",input$show_meas)])
  }, options = list(orderClasses = TRUE))
  
  # customize the length drop-down menu; display 5 rows per page by default
  output$mytable3 = renderDataTable({
    allmeas
  }, options = list(orderClasses = TRUE))
  
  
  myText <- renderText({ " Leave this blank " })
  
  # input$date and others are Date objects. When outputting
  # text, we need to convert to character; otherwise it will
  # print an integer rather than a date.
  
  output$dateText <- renderText({
    paste("input$date is", as.character(input$dataRange))
  })
  
  output$dateText2 <- renderText({
    paste("input$date2 is", as.character(input$date2))
  })
  
  output$dateRangeText  <- renderText({
    paste("input$dateRange is", 
          paste(as.character(input$dateRange), collapse = " to ")
    )
  })
  
  output$dateRangeText2 <- renderText({
    paste("input$dateRange2 is", 
          paste(as.character(input$dateRange2), collapse = " to ")
    )
  })
  
  # Function that generates scenarios. The expression 
  # is wrapped in a call to reactive to indicate that: 
  # 
  #  1) It is "reactive" and therefore should be automatically 
  #     re-executed when inputs change 
  #   2) Its output type is a plot 
  
  
  ## HACK FLO
  
  callModule(mydygraphModule,"dygraph1") 

  callModule(mydygraphModule,"dygraph2")
  callModule(mydygraphModule,"dygraph3") 
  
  
  ## HACK FLO END
  
  
  

  
  
  
  
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
  # Show the first "n" observations
  output$view <- renderTable({
    head(datasetInput(), n = input$cat)
  })
  
  
  
  
  
}