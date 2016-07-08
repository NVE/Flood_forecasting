
library(shiny)
#setwd("D:\\workspace\\DDM\\ML")
#load("hbvAll.rda")
#load("StationsAll.rda")

#models<-c("AllModels","HBV","DDD","NeuralNetwork","SupportVectorMachine", "Generlised Boosted Regression","Model Trees","Model Trees -Cubic")
#models<-names(alldat)[4:9]
#plotTypes<-c("Ordinary Hydrograph","Taylor Diagram","Performance Indicators" )

shinyServer(function(input, output,session) {
  
  filename=reactive({
    paste0(input$torre,input$tipo)
  })
  day1=reactive({
    as.POSIXct(input$date1)
  })
  day2=reactive({
    as.POSIXct(input$date2)    
  })
 
  library(zoo)
  var <- reactive({
    input$catch
  })
  
#   myDat<-renderDataTable(alldat)
#   myDat<-renderTable({
#     #myDat<-alldat
#     myDat <- myDat[myDat$Catchment==input$cat,]
#     a<-myDat
#     return(a)
#   })
  
#   allDat<-renderTable({alldat})
#   myCat<-reactive({input$cat})
#     #myDat<-renderTable({
#       #myDat<-alldat
#       myDat <- subset(allDat,Catchment==myCat())
#       a<-myDat
#       return(a)
# #    })
    
    
    # myDats<-reactive({
    #   b<-subset(alldat,myDate < as.Date("1980-06-10") & myDate > as.Date("1980-05-15") & Catchment=="2.11_Narsjo")
    #   return(b)
    # })
    
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
      library(ggplot2)
      #diamonds[, input$show_vars, drop = FALSE]
      allmeas
    })
    library(DT)
    
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
  output$dateText  <- renderText({
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
  
  
  
  # Expression that plots. The expression 
  # is wrapped in a call to renderPlot to indicate that: 
  # 
  #  1) It is "reactive" and therefore should be automatically 
  #     re-executed when inputs change 
  #  2) Its output type is a plot 
  # 
  
  
  output$Allplot <- renderPlot({ 
    ggplot(data = subset(mydat, Catchment %in% input$catch & model %in% "Observed")) +
      geom_line(aes(x = myDate, y = flow), size = 0.7) +
      geom_line(data = subset(mydat, Catchment %in% input$catch), aes(x = myDate, y = flow, group = model, col = model), size = 0.4)
        }) 
  output$zoomPlot <- renderPlot({ 
    ggplot(data = subset(mydat, Catchment %in% input$catch & model %in% "Observed" & myDate > as.character(input$dateRange[1]) & myDate <as.character(input$dateRange[2]))) +
      geom_line(aes(x = myDate, y = flow), size = 1) +
      geom_line(data = subset(mydat, Catchment %in% input$catch & myDate > as.character(input$dateRange[1]) & myDate <as.character(input$dateRange[2])), aes(x = myDate, y = flow, group = model, col = model), size = 0.4) 
       }) 
  output$TDplot <- renderPlot({ 
    library(plotrix)
    taylor.diagram(coredata(dat.z[,1] ), coredata(dat.z[,2]), pos.cor = TRUE, main = paste("Taylor Diagram for ", input$catch, sep = ""), ngamma = 6, sd.arcs = 3, ref.sd = TRUE, grad.corr.lines = c(0.2, 0.4, 0.6, 0.8, 0.9, 0.95, 0.99))
    # now add the model
    #taylor.diagram(coredata(XX$Qhbv.2.11_narsjo), coredata(all.ts)[,3], add = TRUE, col = "grey", pos.cor = TRUE, ngamma = 6, sd.arcs = 3, ref.sd = TRUE, grad.corr.lines = c(0.2, 0.4, 0.6, 0.8, 0.9, 0.95, 0.99))
    #      taylor.diagram(coredata(all.ts$Observed), coredata(all.ts)[, 4], add = TRUE, col = "blue", pos.cor = TRUE, ngamma = 6, sd.arcs = 3, ref.sd = TRUE, grad.corr.lines = c(0.2, 0.4, 0.6, 0.8, 0.9, 0.95, 0.99))
    #      taylor.diagram(coredata(all.ts$Observed), coredata(all.ts)[, 5], add = TRUE, col = "brown", pos.cor = TRUE, ngamma = 6, sd.arcs = 3, ref.sd = TRUE, grad.corr.lines = c(0.2, 0.4, 0.6, 0.8, 0.9, 0.95, 0.99))
    #      taylor.diagram(coredata(all.ts$Observed), coredata(all.ts)[, 6], add = TRUE, col = "green", pos.cor = TRUE, ngamma = 6, sd.arcs = 3, ref.sd = TRUE, grad.corr.lines = c(0.2, 0.4, 0.6, 0.8, 0.9, 0.95, 0.99))
    #      taylor.diagram(coredata(all.ts$Observed), coredata(all.ts)[, 7], add = TRUE, col = "pink", pos.cor = TRUE, ngamma = 6, sd.arcs = 3, ref.sd = TRUE, grad.corr.lines = c(0.2, 0.4, 0.6, 0.8, 0.9, 0.95, 0.99))
    #      
    #      lpos<-1.5*sd(coredata(all.ts$Observed))
    #      legend(lpos,lpos,legend=c("HBV", "NNET", "SVM","GBM","M5","M5c"),pch=19,col=c("red","grey","blue","brown","green","pink"))  
  }) 
  # Show the first "n" observations
  output$view <- renderTable({
    head(datasetInput(), n = input$cat)
  })
  
  
})


plotAll <- function(mydat){
   library(ggplot2)
  # layout(matrix(c(1,2,1,3),2,2)) 
  # 
  # palette(c("black", "grey50", "grey30", "grey70", "#d9230f")) 
  # 
  # 
  # # plot all scenarios 
  # 
  # 
  # plot(XX[,1], 
  #      type = 'l', lwd = 2, lty = 1, col = "black", 
  #      xlab = 'Years', ylab = 'flows (m^3/s)', 
  #      main = 'HBV Obs / simulated flows') 
  # lines(XX[,2],col="grey50")
      ggplot(data = subset(mydat, Catchment %in% input$catch & model %in% "Observed")) +
      geom_line(aes(x = myDate, y = flow), size = 0.7) +
      geom_line(data = subset(mydat, Catchment %in% input$catch), aes(x = myDate, y = flow, group = model, col = model), size = 0.4)
    } 
   
# plot zoom to periods of interest   
plotZoom <- function(XX) {
   
     plot(XX[,3], type="l",col = "grey", lwd = 10, xlab = "Time", ylab = "Runoff (m^3/s)")
     lines(XX[,4],col="grey50")
#      lines(all.ts[480:530, 2], col = "black", lwd = 2)
#      lines(all.ts[480:530, 3], col = "red", lwd = 2)
#      lines(all.ts[480:530, 4], col = "blue", lwd = 2)
#      lines(all.ts[480:530, 5], col = "green", lwd = 2)
#      lines(all.ts[480:530, 6], col = "pink", lwd = 2)
#      lines(all.ts[480:530, 7], col = "orange", lwd = 2)
#      legend("topright", colnames(all.ts), lty = 1, col = c("grey","black","red","blue","green","pink","orange"), lwd = c(5, 1, 1))
     grid() 
}
  
    
   
  
        # plot Taylor diagram

plotTaylorD <- function(XX) {
     library(plotrix)
     taylor.diagram(coredata(dat.z[,1] ), coredata(dat.z[,2]), pos.cor = TRUE, main = paste("Taylor Diagram for ", input$catch, sep = ""), ngamma = 6, sd.arcs = 3, ref.sd = TRUE, grad.corr.lines = c(0.2, 0.4, 0.6, 0.8, 0.9, 0.95, 0.99))
     # now add the model
     #taylor.diagram(coredata(XX$Qhbv.2.11_narsjo), coredata(all.ts)[,3], add = TRUE, col = "grey", pos.cor = TRUE, ngamma = 6, sd.arcs = 3, ref.sd = TRUE, grad.corr.lines = c(0.2, 0.4, 0.6, 0.8, 0.9, 0.95, 0.99))
#      taylor.diagram(coredata(all.ts$Observed), coredata(all.ts)[, 4], add = TRUE, col = "blue", pos.cor = TRUE, ngamma = 6, sd.arcs = 3, ref.sd = TRUE, grad.corr.lines = c(0.2, 0.4, 0.6, 0.8, 0.9, 0.95, 0.99))
#      taylor.diagram(coredata(all.ts$Observed), coredata(all.ts)[, 5], add = TRUE, col = "brown", pos.cor = TRUE, ngamma = 6, sd.arcs = 3, ref.sd = TRUE, grad.corr.lines = c(0.2, 0.4, 0.6, 0.8, 0.9, 0.95, 0.99))
#      taylor.diagram(coredata(all.ts$Observed), coredata(all.ts)[, 6], add = TRUE, col = "green", pos.cor = TRUE, ngamma = 6, sd.arcs = 3, ref.sd = TRUE, grad.corr.lines = c(0.2, 0.4, 0.6, 0.8, 0.9, 0.95, 0.99))
#      taylor.diagram(coredata(all.ts$Observed), coredata(all.ts)[, 7], add = TRUE, col = "pink", pos.cor = TRUE, ngamma = 6, sd.arcs = 3, ref.sd = TRUE, grad.corr.lines = c(0.2, 0.4, 0.6, 0.8, 0.9, 0.95, 0.99))
#      
#      lpos<-1.5*sd(coredata(all.ts$Observed))
#      legend(lpos,lpos,legend=c("HBV", "NNET", "SVM","GBM","M5","M5c"),pch=19,col=c("red","grey","blue","brown","green","pink"))
#      
#     grid() 
   } 
