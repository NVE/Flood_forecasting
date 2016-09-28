# library(shiny)
library(ggplot2)
library(dygraphs)
library(zoo)
library(DT)
library(plotrix)


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
  
  
  library(DT)
  # Return the requested dataset
  
  choices = setNames(unique(stnDatMaster$stN),unique(stnDatMaster$stN))
  
  datasetInput <- reactive({
    
    selectInput("model", "Select your choice",  choices)
  })
  
  library(DT)
  
  output$table0<-renderTable({
    ddmFdataM
  })
  
  # hits & false alarms
  
  output$tablehits<-renderDataTable({
    htab0<-subset(stnDatMaster, stnDatMaster$stN %in% input$catch & stnDatMaster$Date > as.character(input$dateRange[1]) & stnDatMaster$Date < as.character(input$dateRange[2]))
    htab<-zoo(htab0[,4:10],htab0$Date)
    htab <- htab[complete.cases(htab0), ]
    
    myhtab <- hrs.f(obs=htab[,1], sim=htab[,2])
    myhtab <- rbind(myhtab, hrs.f(obs=htab[,1], sim=htab[,3]))
    myhtab <- rbind(myhtab, hrs.f(obs=htab[,1], sim=htab[,4]))
    myhtab <- rbind(myhtab, hrs.f(obs=htab[,1], sim=htab[,5]))
    myhtab <- rbind(myhtab, hrs.f(obs=htab[,1], sim=htab[,6]))
    myhtab <- rbind(myhtab, hrs.f(obs=htab[,1], sim=htab[,7]))
    myhtab<-head(myhtab)
  })
  
  
  # choose columns to display
  # sorted columns are colored now because CSS are attached to them
  
  output$mytable = renderDataTable({
    toEval<-subset(stnDatMaster, stnDatMaster$stN %in% input$catch & stnDatMaster$Date > as.character(input$dateRange[1]) & stnDatMaster$Date < as.character(input$dateRange[2]))
    toEval <- toEval[complete.cases(toEval), ]
    evTable0 <-rbind( 
      as.data.frame(cbind(
        round(cor(toEval[,4],toEval[,5],use="pairwise.complete.obs"),3),
        round(rmse(toEval[,4],toEval[,5],use="pairwise.complete.obs"),3),
        round(NSE(toEval[,4],toEval[,5],use="pairwise.complete.obs"),3),
        round(KGE(toEval[,4],toEval[,5],use="pairwise.complete.obs"),3),
        round(VE(toEval[,4],toEval[,5],use="pairwise.complete.obs"),3))),
      as.data.frame(cbind(
        round(cor(toEval[,4],toEval[,6],use="pairwise.complete.obs"),3),
        round(rmse(toEval[,4],toEval[,6],use="pairwise.complete.obs"),3),
        round(NSE(toEval[,4],toEval[,6],use="pairwise.complete.obs"),3),
        round(KGE(toEval[,4],toEval[,6],use="pairwise.complete.obs"),3),
        round(VE(toEval[,4],toEval[,6],use="pairwise.complete.obs"),3))), 
      as.data.frame(cbind(
        round(cor(toEval[,4],toEval[,7],use="pairwise.complete.obs"),3),
        round(rmse(toEval[,4],toEval[,7],use="pairwise.complete.obs"),3),
        round(NSE(toEval[,4],toEval[,7],use="pairwise.complete.obs"),3),
        round(KGE(toEval[,4],toEval[,7],use="pairwise.complete.obs"),3),
        round(VE(toEval[,4],toEval[,7],use="pairwise.complete.obs"),3))),
      as.data.frame(cbind(
        round(cor(toEval[,4],toEval[,8],use="pairwise.complete.obs"),3),
        round(rmse(toEval[,4],toEval[,8],use="pairwise.complete.obs"),3),
        round(NSE(toEval[,4],toEval[,8],use="pairwise.complete.obs"),3),
        round(KGE(toEval[,4],toEval[,8],use="pairwise.complete.obs"),3),
        round(VE(toEval[,4],toEval[,8],use="pairwise.complete.obs"),3))),
      as.data.frame(cbind(
        round(cor(toEval[,4],toEval[,9],use="pairwise.complete.obs"),3),
        round(rmse(toEval[,4],toEval[,9],use="pairwise.complete.obs"),3),
        round(NSE(toEval[,4],toEval[,9],use="pairwise.complete.obs"),3),
        round(KGE(toEval[,4],toEval[,9],use="pairwise.complete.obs"),3),
        round(VE(toEval[,4],toEval[,9],use="pairwise.complete.obs"),3))),
      as.data.frame(cbind(
        round(cor(toEval[,4],toEval[,10],use="pairwise.complete.obs"),3),
        round(rmse(toEval[,4],toEval[,10],use="pairwise.complete.obs"),3),
        round(NSE(toEval[,4],toEval[,10],use="pairwise.complete.obs"),3),
        round(KGE(toEval[,4],toEval[,10],use="pairwise.complete.obs"),3),
        round(VE(toEval[,4],toEval[,10],use="pairwise.complete.obs"),3))))
    
    evTable<-cbind(c("HBV","NNET","SVM","GBM","M5n","M5c"),evTable0)
    names(evTable)<-c("model","COR","RMSE","NSE","KGE","VE")
    
    #mymeas<-DT::datatable(evTable[ ,c("Model",input$show_meas)])
    evtable1<-DT::datatable(evTable[evTable$model ,input$show_meas,drop=FALSE])
    evtable1
    
  })
  
  # Forecasting plot and table 
  
  output$mytable1 = DT::renderDataTable({
    lineNo<-which(forecastDat[,2]==paste("Felt:",input$catch,sep=" "))
    stnForecast<-forecastDat[(lineNo+3):(lineNo+33),]
    stnForecast$nedb <-as.numeric(as.character(stnForecast$nedb))
    stnForecast$temp <-as.numeric(as.character(stnForecast$temp))
    stnForecast$Obs <-as.numeric(as.character(stnForecast$Obs))
    stnForecast$gbm <-round(as.numeric(as.character(stnForecast$gbm)),3)
    stnForecast$m5c <-round(as.numeric(as.character(stnForecast$m5c)),3)
    stnForecast$nnet <-round(as.numeric(as.character(stnForecast$nnet)),3)
    stnForecast$svm <-round(as.numeric(as.character(stnForecast$svm)),3)
    stnForecast$dates<-as.Date(stnForecast[,1],format="%Y-%m-%d")
    
    stnForecast1<-stnForecast[(dim(stnForecast)[1]-8):(dim(stnForecast)[1]),-9]
    row.names(stnForecast1)<-NULL
    stnForecast1
  })
  
  #the whole month table
  
  output$mytable2 = DT::renderDataTable({
    lineNo<-which(forecastDat[,2]==paste("Felt:",input$catch,sep=" "))
    stnForecast<-forecastDat[(lineNo+3):(lineNo+33),]
    stnForecast$nedb <-as.numeric(as.character(stnForecast$nedb))
    stnForecast$temp <-as.numeric(as.character(stnForecast$temp))
    stnForecast$Obs <-as.numeric(as.character(stnForecast$Obs))
    stnForecast$gbm <-round(as.numeric(as.character(stnForecast$gbm)),3)
    stnForecast$m5c <-round(as.numeric(as.character(stnForecast$m5c)),3)
    stnForecast$nnet <-round(as.numeric(as.character(stnForecast$nnet)),3)
    stnForecast$svm <-round(as.numeric(as.character(stnForecast$svm)),3)
    stnForecast$dates<-as.Date(stnForecast[,1],format="%Y-%m-%d")
    
    #stnForecast2<-stnForecast[(1:dim(stnForecast)[1]-8):(dim(stnForecast)[1]),]
    stnForecast2<-stnForecast[,-9]
    row.names(stnForecast2)<-NULL
    stnForecast2
    
  })
  
  
  
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
  
  library(dygraphs)
  output$mydygraph <- renderDygraph({
    
    # dat1<-subset(alldat, Catchment %in% input$catch)
    mdat1<-subset(stnDatMaster, stN %in% input$catch)
    mdat1<-mdat1[,-11]
    
    mdat.z<-zoo(mdat1[,2:3],mdat1$Date)
    mdat.z1<-zoo(mdat1[,4:10],mdat1$Date)
    mmyts<-as.ts(mdat.z)
    mmyts1<-as.ts(mdat.z1)
    
    # dygraph(
    #   mmyts, main= "Rainfall Temperature", group="myTS") %>% 
    #   dySeries("Rain", stepPlot = TRUE, fillGraph = TRUE, color = "blue",label = "Rainfall (mm)")%>%dyLegend(show = "follow") %>%
    #   dySeries("Temp",  color = "red",axis = 'y2',label = "Temperature (oC)") %>%dyLegend(show = "follow")
    
    dygraph(
      mmyts1, main="Flows",group="myTS") %>% dyOptions(fillGraph = TRUE, fillAlpha = 0.2)%>%dyHighlight(highlightCircleSize = 5, 
                                                                                                        highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = FALSE)%>%dyLegend(show = "follow")
    #myts#%>%dyRangeSelector()
    
  })
  
  #Forecasting plot is created here
  
  output$mygraph2 <- renderPlot({
    # start dygraph with all the states
    lineNo<-which(forecastDat[,2]==paste("Felt:",input$catch,sep=" "))
    stnForecast<-forecastDat[(lineNo+3):(lineNo+33),]
    stnForecast$nedb <-as.numeric(as.character(stnForecast$nedb))
    stnForecast$temp <-as.numeric(as.character(stnForecast$temp))
    stnForecast$Obs <-as.numeric(as.character(stnForecast$Obs))
    stnForecast$gbm <-as.numeric(as.character(stnForecast$gbm))
    stnForecast$m5c <-as.numeric(as.character(stnForecast$m5c))
    stnForecast$nnet <-as.numeric(as.character(stnForecast$nnet))
    stnForecast$svm <-as.numeric(as.character(stnForecast$svm))
    stnForecast$dates<-as.Date(stnForecast[,1],format="%Y-%m-%d")
    
    #limits for plotting
    ymx<-max(stnForecast[,4:8],na.rm=TRUE)+1
    ymn<-min(stnForecast[,4:8],na.rm=TRUE)-0.5
    
    #making time series
    foreZoo<-zoo(stnForecast[,2:3],stnForecast$dato)
    ForeTs<-as.ts(foreZoo)
    foreZoo2<-zoo(stnForecast[,4:8],stnForecast[,1])
    ForeTs2<-as.ts(foreZoo2)
    
    #for making the box -rectangle shading 
    par(mar=c(5,4,4,4)+0.1)
    plot(stnForecast$temp,type="l",yaxt = "n", space = NULL, col="brown",ylim = rev(c(0, 4 * max(na.omit(stnForecast$temp)))),xaxt = "n")
    par(new=TRUE)
    barplot(stnForecast$nedb, yaxt = "n", space = NULL, col="lightblue",border = NA,
            ylim = rev(c(0, 4 * max(na.omit(stnForecast$nedb)))),xaxt = "n",ylab = "Stremflow -  m^3/s")
    axis(side = 3, pos = 0, tck = 0,xaxt = "n")
    axis(side = 4, at = seq(0, floor(max(na.omit(stnForecast$nedb)) + 
                                       1), length = (1 + ifelse(floor(max(na.omit(stnForecast$nedb)) + 
                                                                        1) < 10, floor(max(na.omit(stnForecast$nedb)) + 1), 
                                                                4))), labels = as.integer(seq(0, floor(max(na.omit(stnForecast$nedb)) + 
                                                                                                         1), length = (1 + ifelse(floor(max(na.omit(stnForecast$nedb)) + 
                                                                                                                                          1) < 10, floor(max(na.omit(stnForecast$nedb)) + 1), 
                                                                                                                                  4)))))
    mtext("Precip                            ", side=4, line = 1.1, cex = 1.2, adj = 1)
    par(new=TRUE)
    plot(foreZoo2[,1],ylim=c(ymn,ymx),cex.main = 0.8, type = "l", col = "black", lwd=0.8, ylab="",xlab="Dates")
    rect(2,-4,4,4,col = rgb(0.5,0.5,0.5,1/4))
    lines(foreZoo2[23:31,2],col="blue", lty=2, lwd=2)
    lines(foreZoo2[23:31,3],col="red", lty=2, lwd=2)
    lines(foreZoo2[23:31,4],col="green", lty=2, lwd=2)
    lines(foreZoo2[23:31,5],col="lightblue", lty=2, lwd=2)
    rect(index(foreZoo2)[22],ymn,index(foreZoo2)[31],ymx,col = rgb(0.5,0.5,0.5,1/4)) #### shading
    
  })
  
  output$mygraph3 <- renderPlot({
    # start dygraph with all the states
    lineNo<-which(forecastDat[,2]==paste("Felt:",input$catch,sep=" "))
    stnForecast<-forecastDat[(lineNo+3):(lineNo+33),]
    stnForecast$nedb <-as.numeric(as.character(stnForecast$nedb))
    stnForecast$temp <-as.numeric(as.character(stnForecast$temp))
    stnForecast$Obs <-as.numeric(as.character(stnForecast$Obs))
    stnForecast$gbm <-as.numeric(as.character(stnForecast$gbm))
    stnForecast$m5c <-as.numeric(as.character(stnForecast$m5c))
    stnForecast$nnet <-as.numeric(as.character(stnForecast$nnet))
    stnForecast$svm <-as.numeric(as.character(stnForecast$svm))
    stnForecast$dates<-as.Date(stnForecast[,1],format="%Y-%m-%d")
    
    #limits for plotting
    ymx<-max(stnForecast[23:31,4:8],na.rm=TRUE)+0.5
    ymn<-min(stnForecast[23:31,4:8],na.rm=TRUE)-0.5
    
    #making time series
    foreZoo<-zoo(stnForecast[,2:3],stnForecast$dato)
    ForeTs<-as.ts(foreZoo)
    foreZoo2<-zoo(stnForecast[,4:8],stnForecast[,1])
    ForeTs2<-as.ts(foreZoo2)
    
    #for making the box -rectangle shading 
    par(mar=c(5,4,4,4)+0.1)
    plot(stnForecast[23:31,3],type="l",yaxt = "n", space = NULL, col="brown",ylim = rev(c(0, 4 * max(na.omit(stnForecast[23:31,3])))),xaxt = "n",ylab = "")
    par(new=TRUE)
    barplot(stnForecast[23:31,2], yaxt = "n", space = NULL, col="lightblue",border = NA,
            ylim = rev(c(0, 4 * max(na.omit(stnForecast[23:31,2])))),xaxt = "n",ylab = "Stremflow -  m^3/s")
    #axis(side = 3, pos = 0, tck = 0,xaxt = "n")
    axis(side = 4, at = seq(0, floor(max(na.omit(stnForecast$nedb)) + 
                                       1), length = (1 + ifelse(floor(max(na.omit(stnForecast$nedb)) + 
                                                                        1) < 10, floor(max(na.omit(stnForecast$nedb)) + 1), 
                                                                4))), labels = as.integer(seq(0, floor(max(na.omit(stnForecast$nedb)) + 
                                                                                                         1), length = (1 + ifelse(floor(max(na.omit(stnForecast$nedb)) + 
                                                                                                                                          1) < 10, floor(max(na.omit(stnForecast$nedb)) + 1), 
                                                                                                                                  4)))))
    mtext("Precip                            ", side=4, line = 1.1, cex = 1.2, adj = 1)
    
    par(new=TRUE)
    plot(foreZoo2[23:31,1],ylim=c(ymn,ymx),cex.main = 0.8, type = "l", col = "white", lwd=0.8, ylab="",xlab="Dates")
    rect(2,-4,4,4,col = rgb(0.5,0.5,0.5,1/4))
    lines(foreZoo2[23:31,2],col="blue", lty=2, lwd=2)
    lines(foreZoo2[23:31,3],col="red", lty=2, lwd=2)
    lines(foreZoo2[23:31,4],col="green", lty=2, lwd=2)
    lines(foreZoo2[23:31,5],col="lightblue", lty=2, lwd=2)
    rect(index(foreZoo2)[22],ymn,index(foreZoo2)[31],ymx,col = rgb(0.5,0.5,0.5,1/4)) #### shading
    
  })
  
  
  output$TDplot <- renderPlot({ 
    library(plotrix)
    mydat0<-subset(stnDatMaster,  stN %in% input$catch &  Date > as.character(input$dateRange[1]) & Date <as.character(input$dateRange[2]))
    # now add the model
    taylor.diagram(mydat0[,4], mydat0[,5], pos.cor = TRUE, main = paste("Taylor Diagram for ", input$catch, sep = ""), ngamma = 6, sd.arcs = 3, ref.sd = TRUE, grad.corr.lines = c(0.2, 0.4, 0.6, 0.8, 0.9, 0.95, 0.99))
    
    taylor.diagram(mydat0[,4], mydat0[,6], add = TRUE, col = "grey", pos.cor = TRUE, ngamma = 6, sd.arcs = 3, ref.sd = TRUE, grad.corr.lines = c(0.2, 0.4, 0.6, 0.8, 0.9, 0.95, 0.99))
    taylor.diagram(mydat0[,4], mydat0[,6], add = TRUE, col = "blue", pos.cor = TRUE, ngamma = 6, sd.arcs = 3, ref.sd = TRUE, grad.corr.lines = c(0.2, 0.4, 0.6, 0.8, 0.9, 0.95, 0.99))
    taylor.diagram(mydat0[,4], mydat0[,8], add = TRUE, col = "brown", pos.cor = TRUE, ngamma = 6, sd.arcs = 3, ref.sd = TRUE, grad.corr.lines = c(0.2, 0.4, 0.6, 0.8, 0.9, 0.95, 0.99))
    taylor.diagram(mydat0[,4], mydat0[,9], add = TRUE, col = "green", pos.cor = TRUE, ngamma = 6, sd.arcs = 3, ref.sd = TRUE, grad.corr.lines = c(0.2, 0.4, 0.6, 0.8, 0.9, 0.95, 0.99))
    taylor.diagram(mydat0[,4], mydat0[,10], add = TRUE, col = "green", pos.cor = TRUE, ngamma = 6, sd.arcs = 3, ref.sd = TRUE, grad.corr.lines = c(0.2, 0.4, 0.6, 0.8, 0.9, 0.95, 0.99))     
    lpos<-1.4*sd(mydat0$Observed)
    legend(lpos,lpos,legend=c("HBV", "NNET", "SVM","GBM","M5","M5c"),pch=19,col=c("red","grey","blue","brown","green","pink"))  
  }) 
  # Show the first "n" observations
  output$view <- renderDataTable({
    perform<-evTable
    
  },include.rownames=FALSE)
  
  
})
