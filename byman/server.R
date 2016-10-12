


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
 
  var <- reactive({
    input$catch
  })
  

# Return the requested dataset
    
  choices = setNames(unique(stnDatMaster$stN),unique(stnDatMaster$stN))
  
  datasetInput <- reactive({
    
   selectInput("model", "Select your choice",  choices)
    })
  
    output$table0<-renderTable({
      ddmFdataM
    })
    
 # hits & false alarms
    
    output$tablehits<-renderDataTable({
      htab0<-subset(stnDatMaster, stnDatMaster$stN %in% input$catch & stnDatMaster$Date > as.character(input$dateRange[1]) & stnDatMaster$Date < as.character(input$dateRange[2]))
      htab<-zoo(htab0[,4:10],htab0$Date)
      htab <- htab[complete.cases(htab0), ]
      
      myhtab <- as.data.frame(hrs.f(obs=htab[,1], sim=htab[,2]))
      myhtab <- rbind(as.data.frame(myhtab), as.data.frame(hrs.f(obs=htab[,1], sim=htab[,3])))
      myhtab <- rbind(as.data.frame(myhtab), as.data.frame(hrs.f(obs=htab[,1], sim=htab[,4])))
      myhtab <- rbind(as.data.frame(myhtab), as.data.frame(hrs.f(obs=htab[,1], sim=htab[,5])))
      myhtab <- rbind(as.data.frame(myhtab), as.data.frame(hrs.f(obs=htab[,1], sim=htab[,6])))
      myhtab <- rbind(as.data.frame(myhtab), as.data.frame(hrs.f(obs=htab[,1], sim=htab[,7])))
      myhtab$HR<-round(myhtab$HR,3)
      myhtab$FAR<-round(myhtab$FAR,3)
      myhtab$SI<-round(myhtab$SI,3)
      row.names(myhtab)[1]<-names(htab)[2]
      row.names(myhtab)[2]<-names(htab)[3]
      row.names(myhtab)[3]<-names(htab)[4]
      row.names(myhtab)[4]<-names(htab)[5]
      row.names(myhtab)[5]<-names(htab)[6]
      row.names(myhtab)[6]<-names(htab)[7]
      myhtab<-DT::datatable(myhtab,
                              options = list(
                                initComplete = JS(
                                  "function(settings, json) {",
                                  "$(this.api().table().header()).css({'background-color': 'grey', 'color': '#fff'});",
                                  "}")
                              ))%>%
        formatStyle('HR', fontWeight = styleInterval(5, c('normal','bold')))%>%
        formatStyle('HR',color = styleInterval(c(0,0.2,0.5,1), c( 'red','brown','orange','blue','green')),
                    backgroundColor = styleInterval(1, c('gray95', 'gray50')))%>%
        formatStyle('FAR', fontWeight = styleInterval(5, c('normal','bold')))%>%
        formatStyle('FAR',color = styleInterval(c(0.1,0.5,0.8,0.9), c( 'red','brown','orange','blue','green')),
                    backgroundColor = styleInterval(1, c('gray95', 'gray50')))%>%
        formatStyle('SI', fontWeight = styleInterval(5, c('normal','bold')))%>%
        formatStyle('SI',color = styleInterval(c(0.1,0.5,0.8,0.9), c( 'red','brown','orange','blue','green')),
                    backgroundColor = styleInterval(1, c('gray95', 'gray50')))
      myhtab
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
          round(pbias(toEval[,4],toEval[,5],use="pairwise.complete.obs"),3))),
        as.data.frame(cbind(
          round(cor(toEval[,4],toEval[,6],use="pairwise.complete.obs"),3),
          round(rmse(toEval[,4],toEval[,6],use="pairwise.complete.obs"),3),
          round(NSE(toEval[,4],toEval[,6],use="pairwise.complete.obs"),3),
          round(KGE(toEval[,4],toEval[,6],use="pairwise.complete.obs"),3),
          round(pbias(toEval[,4],toEval[,6],use="pairwise.complete.obs"),3))), 
        as.data.frame(cbind(
          round(cor(toEval[,4],toEval[,7],use="pairwise.complete.obs"),3),
          round(rmse(toEval[,4],toEval[,7],use="pairwise.complete.obs"),3),
          round(NSE(toEval[,4],toEval[,7],use="pairwise.complete.obs"),3),
          round(KGE(toEval[,4],toEval[,7],use="pairwise.complete.obs"),3),
          round(pbias(toEval[,4],toEval[,7],use="pairwise.complete.obs"),3))),
        as.data.frame(cbind(
          round(cor(toEval[,4],toEval[,8],use="pairwise.complete.obs"),3),
          round(rmse(toEval[,4],toEval[,8],use="pairwise.complete.obs"),3),
          round(NSE(toEval[,4],toEval[,8],use="pairwise.complete.obs"),3),
          round(KGE(toEval[,4],toEval[,8],use="pairwise.complete.obs"),3),
          round(pbias(toEval[,4],toEval[,8],use="pairwise.complete.obs"),3))),
        as.data.frame(cbind(
          round(cor(toEval[,4],toEval[,9],use="pairwise.complete.obs"),3),
          round(rmse(toEval[,4],toEval[,9],use="pairwise.complete.obs"),3),
          round(NSE(toEval[,4],toEval[,9],use="pairwise.complete.obs"),3),
          round(KGE(toEval[,4],toEval[,9],use="pairwise.complete.obs"),3),
          round(pbias(toEval[,4],toEval[,9],use="pairwise.complete.obs"),3))),
        as.data.frame(cbind(
          round(cor(toEval[,4],toEval[,10],use="pairwise.complete.obs"),3),
          round(rmse(toEval[,4],toEval[,10],use="pairwise.complete.obs"),3),
          round(NSE(toEval[,4],toEval[,10],use="pairwise.complete.obs"),3),
          round(KGE(toEval[,4],toEval[,10],use="pairwise.complete.obs"),3),
          round(pbias(toEval[,4],toEval[,10],use="pairwise.complete.obs"),3))))
      
      evTable<-cbind(c("NNET","SVM","GBM","M5n","M5c","HBV"),evTable0)
      names(evTable)<-c("model","COR","RMSE","NSE","KGE","BIAS-%")
      rownames(evTable) <- NULL
      
      evtable1<-DT::datatable(evTable[evTable$model,input$show_meas,drop=FALSE],rownames = FALSE,
                              options = list(
                                initComplete = JS(
                                  "function(settings, json) {",
                                  "$(this.api().table().header()).css({'background-color': 'grey', 'color': '#fff'});",
                                  "}")
                              ))%>%formatStyle('model',  color = 'white', backgroundColor = 'grey', fontWeight = 'bold')%>%
                              formatStyle('RMSE', fontWeight = styleInterval(5, c('normal','bold')))%>%
                              formatStyle('RMSE',color = styleInterval(c(0,0.2,0.5,1), c( 'green','lighblue','orange','brown','red')),
          backgroundColor = styleInterval(1, c('gray95', 'gray50')))%>%
        formatStyle('NSE', fontWeight = styleInterval(5, c('normal','bold')))%>%
        formatStyle('NSE',color = styleInterval(c(0.6,0.75,0.85,0.92), c( 'red','brown','orange','blue','green')),
                    backgroundColor = styleInterval(1, c('gray95', 'gray50')))%>%
        formatStyle('KGE', fontWeight = styleInterval(5, c('normal','bold')))%>%
        formatStyle('KGE',color = styleInterval(c(0.9,0.92,0.95,0.98), c( 'red','brown','orange','blue','green')),
                    backgroundColor = styleInterval(1, c('gray95', 'gray50')))%>%
       formatStyle('BIAS-%', fontWeight = styleInterval(5, c('normal','bold')))%>%
       formatStyle('BIAS-%',color = styleInterval(c(0,1,5,10), c('red','brown','orange','blue','green')),
                   backgroundColor = styleInterval(1, c('gray95', 'gray50')))
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
        stnForecast1<-DT::datatable(stnForecast1,rownames = FALSE,
                                    options = list(
                                      initComplete = JS(
                                        "function(settings, json) {",
                                        "$(this.api().table().header()).css({'background-color': 'grey', 'color': '#fff'});",
                                        "}")
                                    ))%>%formatStyle('dato',  color = 'white', backgroundColor = 'grey', fontWeight = 'bold')
        
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
      
      stnForecast2<-stnForecast[,-9]
      stnForecast2<-DT::datatable(stnForecast2,rownames = FALSE,options = list(
        initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'background-color': 'grey', 'color': '#fff'});",
          "}")
      ))%>%formatStyle('dato',  color = 'white', backgroundColor = 'grey', fontWeight = 'bold')
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
    
  
  output$mydygraph <- renderDygraph({
   
    mdat1<-subset(stnDatMaster, stN %in% input$catch &  Date > as.character(input$dateRange[1]) & Date <as.character(input$dateRange[2]))
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
              highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = FALSE)%>%dyLegend(show = "follow")#%>%dyRangeSelector())
      #myts#%>%dyRangeSelector()
    
  })
  
#Forecasting plot is created here ------------------------------------------------------------
  
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
     barplot(stnForecast$nedb, yaxt = "n", space = NULL, col="lightblue",border = NA,
             ylim = rev(c(0, 4 * max(na.omit(stnForecast$nedb)))),xaxt = "n",ylab = "")
     par(new=TRUE)
     plot(stnForecast$temp,type="l",lwd=0.4, yaxt = "n", space = NULL, col="brown",ylim = rev(c(0, 4 * max(na.omit(stnForecast$temp)))),xaxt = "n",ylab = "Streamflow m^3/s",xlab = "Dates")
    
     axis(side = 3, pos = 0, tck = 0,xaxt = "n")
     axis(side = 4, at = seq(0, floor(max(na.omit(stnForecast$nedb)) + 
                                        1), length = (1 + ifelse(floor(max(na.omit(stnForecast$nedb)) + 
                                                                         1) < 10, floor(max(na.omit(stnForecast$nedb)) + 1), 
                                                                 4))), labels = as.integer(seq(0, floor(max(na.omit(stnForecast$nedb)) + 
                                                                                                          1), length = (1 + ifelse(floor(max(na.omit(stnForecast$nedb)) + 
                                                                                                                                           1) < 10, floor(max(na.omit(stnForecast$nedb)) + 1), 
                                                                                                                                   4)))))
     mtext("Precip                                ", side=4, line = 1.1, cex = 1.2, adj = 1)
     par(new=TRUE)
     plot(foreZoo2[,1],ylim=c(ymn,ymx),cex.main = 0.8, type = "l", col = "black", lwd=0.8, ylab="",xlab="")
     
     rect(2,-4,4,4,col = rgb(0.5,0.5,0.5,1/4))
     
     lines(foreZoo2[23:30,2],col="blue", lty=2, type="b", lwd=0.6,pch=18)
     
     lines(foreZoo2[23:30,3],col="green", lty=3, type="b", lwd=0.6,pch=19)
     
     lines(foreZoo2[23:30,4],col="red", lty=4, type="b", lwd=0.6,pch=20)
    
     lines(foreZoo2[23:30,5],col="purple", lty=5, type="b", lwd=0.6,pch=21)
     
     xb<-c(Sys.Date(),Sys.Date()+11,Sys.Date()+11,Sys.Date())
     
     yb<-c(-0.9,-0.9,4 * max(na.omit(stnForecast$temp)),4 * max(na.omit(stnForecast$temp)))
     
     polygon(xb, yb, col = "#FFF8DC75",border='grey96')
     
     legend(x="topleft",bty = "n",horiz = TRUE,cex=0.9,pt.cex = 0.8,
            pch = c(15, 32), 
            col = c("lightblue", "brown"), 
            lwd=0.4, lty=c(0,1),
            legend = c("Rainfall", "Temp"))
     
     legend( x="bottomleft", bty = "n",horiz = TRUE,cex=0.9,pt.cex = 0.8,
     legend=c("Observed","NNET","SVM","GBM","M5c"),
     col=c("black","blue","green","red","purple"), lwd=0.4, lty=c(1,2,3,4,5),
     pch=c(1,18,19,20,21) )
      
  })
  
   # start dygraph with all the states
  output$mygraph3 <- renderPlot({
   
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
    #for making the box -rectangle shading 
    par(mar=c(5,4,4,4)+0.1)
   
    barplot(stnForecast[23:31,2], yaxt = "n", space = NULL, col="lightblue",border = NA,
            ylim = rev(c(0, 4 * max(na.omit(stnForecast[23:31,2])))),xaxt = "n",ylab = "")
    par(new=TRUE)
    plot(stnForecast[23:31,3],type="l",lwd=0.4,yaxt = "n", space = NULL, col="brown",ylim = rev(c(0, 4 * max(na.omit(stnForecast[23:31,3])))),xaxt = "n",ylab = "Streamflow m^3/s",xlab = "Dates")
    
    #axis(side = 3, pos = 0, tck = 0,xaxt = "n")
    axis(side = 4, at = seq(0, floor(max(na.omit(stnForecast$nedb)) + 
                                       1), length = (1 + ifelse(floor(max(na.omit(stnForecast$nedb)) + 
                                                                        1) < 10, floor(max(na.omit(stnForecast$nedb)) + 1), 
                                                                4))), labels = as.integer(seq(0, floor(max(na.omit(stnForecast$nedb)) + 
                                                                                                         1), length = (1 + ifelse(floor(max(na.omit(stnForecast$nedb)) + 
                                                                                                                                          1) < 10, floor(max(na.omit(stnForecast$nedb)) + 1), 
                                                                                                                                  4)))))
    mtext("Precip                              ", side=4, line = 1.1, cex = 1.2, adj = 1)
    
    par(new=TRUE)
    plot(foreZoo2[23:31,1],ylim=c(ymn,ymx),cex.main = 0.8, type = "l", col = "brown", lwd=0.4, ylab="",xlab="Dates")
    rect(2,-4,4,4,col = rgb(0.5,0.5,0.5,1/4))
    lines(foreZoo2[23:30,2],col="blue", lty=2, type="b", lwd=0.6,pch=18)
    
    lines(foreZoo2[23:30,3],col="green", lty=3, type="b", lwd=0.6,pch=19)
    
    lines(foreZoo2[23:30,4],col="red", lty=4, type="b", lwd=0.6,pch=20)
    
    lines(foreZoo2[23:30,5],col="purple", lty=5, type="b", lwd=0.6,pch=21)
    
    #rect(index(foreZoo2)[22],ymn,index(foreZoo2)[31],ymx,col = rgb(0.5,0.5,0.5,1/4)) #### shading
    
    legend(x="topleft",bty = "n",horiz = TRUE,cex=0.9,pt.cex = 0.8,
           pch = c(15, 32),
           lwd=0.4, lty=c(0,1),
           col = c("lightblue", "brown"), 
           legend = c("Rainfall", "Temp"))
    
    legend( x="bottomleft", bty = "n",
            legend=c("Observed","NNET","SVM","GBM","M5c"),horiz = TRUE,cex=0.9,pt.cex = 0.8,
            col=c("black","blue","green","red","purple"), lwd=0.4, lty=c(1,2,3,4,5),
            pch=c(1,18,19,20,21) )
    
  })
  
 
  output$TDplot <- renderPlot({ 
    #library(plotrix)
    mydat0<-subset(stnDatMaster,  stN %in% input$catch &  Date > as.character(input$dateRange[1]) & Date <as.character(input$dateRange[2]))
     # now add the model
    taylor.diagram(mydat0[,4], mydat0[,10], pos.cor = TRUE, main = paste("Taylor Diagram for ", input$catch, sep = ""), ngamma = 4, sd.arcs = 2, ref.sd = TRUE, grad.corr.lines = c(0.2, 0.5, 0.8, 0.9, 0.95, 0.99))
   
    taylor.diagram(mydat0[,4], mydat0[,5], add = TRUE, col = "grey", pos.cor = TRUE, ngamma = 4, sd.arcs = 2, ref.sd = TRUE, grad.corr.lines = c( 0.5, 0.8, 0.9, 0.95, 0.99))
          taylor.diagram(mydat0[,4], mydat0[,6], add = TRUE, col = "orange", pos.cor = TRUE, ngamma = 4, sd.arcs = 2, ref.sd = TRUE, grad.corr.lines = c( 0.5, 0.8, 0.9, 0.95, 0.99))
          taylor.diagram(mydat0[,4], mydat0[,7], add = TRUE, col = "brown", pos.cor = TRUE, ngamma = 4, sd.arcs = 2, ref.sd = TRUE, grad.corr.lines = c( 0.5, 0.8, 0.9, 0.95, 0.99))
          taylor.diagram(mydat0[,4], mydat0[,8], add = TRUE, col = "green", pos.cor = TRUE, ngamma = 4, sd.arcs = 2, ref.sd = TRUE, grad.corr.lines = c( 0.5, 0.8, 0.9, 0.95, 0.99))
          taylor.diagram(mydat0[,4], mydat0[,9], add = TRUE, col = "pink", pos.cor = TRUE, ngamma = 4, sd.arcs = 2, ref.sd = TRUE, grad.corr.lines = c( 0.5, 0.8, 0.9, 0.95, 0.99))     
          lpos<-1.4*sd(mydat0$Observed)
          legend(lpos,lpos,legend=c("HBV", "NNET", "SVM","GBM","M5n","M5c"),pch=19,col=c("red","grey","orange","brown","green","pink"))  
  }) 
  # Show the first "n" observations
  output$view <- renderDataTable({
    perform<-evTable
    
  },include.rownames=FALSE)

  #----------------------------------------Temperature
  
  output$annualTemp <- renderPlot({
   
    mTt<-subset(stnDatMaster, stnDatMaster$stN %in% input$catch)
    
    
    mTt1<-mTt[,1:4]
    mTt1$Year<-as.numeric(substr(mTt[,1],1,4))
    mTt1$Month<-as.numeric(substr(mTt[,1],6,7))
    mTt1$Day<-as.numeric(substr(mTt[,1],9,10))
    dat2<-mTt1[,-1]
    
    # rename variables
    names(dat2)[3] <- "Runoff"
    
    
    datT<-cbind.data.frame(data.frame(dat2[,6]),data.frame(dat2[,5]),data.frame(dat2[,4]),data.frame(dat2[,2])); names(datT)<-c( "Day", "Month", "Year","Temp")
    
    # create dataframe that represents 1995-2013 historical data
    Past <- datT %>%
      dplyr::group_by(Year, Day) %>%
      dplyr::arrange(Month) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(Year) %>%
      dplyr::mutate(newDay = seq(1, length(Day))) %>%   # label days as 1:365 (will represent x-axis)         
      dplyr::ungroup() %>%
      dplyr::filter(Temp != -99 & Year != 2011) %>%     # filter out missing data (identified with '-99' value) & current year data
      dplyr::group_by(newDay) %>%
      dplyr::mutate(upper = max(Temp), # identify max value for each day
                    lower = min(Temp), # identify min value for each day
                    avg = mean(Temp),  # calculate mean value for each day
                    se = sd(Temp)/sqrt(length(Temp))) %>%  # calculate standard error of mean
      dplyr::mutate(avg_upper = avg+(2.101*se),  # calculate 95% CI for mean
                    avg_lower = avg-(2.101*se)) %>%  # calculate 95% CI for mean
      dplyr::ungroup()
    
    # create dataframe that represents current year data
    Present <- datT%>%
      group_by(Year, Month)%>%
      arrange(Month)%>%
      ungroup()%>%
      group_by(Year)%>%
      mutate(newDay = seq(1, length(Day)))%>%  # create matching x-axis as historical data
      ungroup()%>%
      filter(Temp != -99 & Year == 2011)  # filter out missing data & select current year data
    
    
    # create dataframe that represents the lowest temp for each day for the historical data
    PastLows <- Past %>%
      group_by(newDay) %>%
      summarise(Pastlow = min(Temp)) # identify lowest temp for each day from 1995-2013
    
    # create dataframe that identifies the days in 2014 in which the temps were lower than all previous 19 years
    PresentLows <- Present %>%
      left_join(PastLows) %>%  # merge historical lows to current year low data
      mutate(record = ifelse(Temp<Pastlow, "Y", "N")) %>% # identifies if current year was record low
      filter(record == "Y")  # filter for days that represent current year record lows
    
    # create dataframe that represents the highest temp for each day for the historical data
    PastHighs <- Past %>%
      group_by(newDay) %>%
      summarise(Pasthigh = max(Temp))  # identify highest temp for each day from 1995-2013
    
    # create dataframe that identifies the days in 2014 in which the temps were higher than all previous 19 years
    PresentHighs <- Present %>%
      left_join(PastHighs) %>%  # merge historical highs to current year low data
      mutate(record = ifelse(Temp>Pasthigh, "Y", "N")) %>% # identifies if current year was record high
      filter(record == "Y") 
    # filter for days that represent current year record highs
    #Lastly, to create the appropriate y-axis, I need to generate a function to turn the y-axis labels into the appropriate format with the degree symbol (o) after the numbers. I then create a variable that represents the y-axis labels I want to display using seq() and apply the degree formatting function to these labels. I save it as variable "a" to be used later in my ggplot code.
    
    #The final data I need to process is to create a small line that will represent the legend symbol for the 2014 Temperature. In Tufte's illustration, the highs and lows data for each day were available so it made sense to represent the current year bars; however, in my case I only have access to the average daily temperature for each day. Since I don't have a range of data for the individual days in 2014, I can only represent the data by a single line. This, ultimately, is why my legend differs slightly compared to the original graphic.
    
    # function to turn y-axis labels into degree formatted values
    dgr_fmt <- function(x, ...) {
      parse(text = paste(x, "*degree", sep = ""))
    }
    
    # create y-axis variable
    a <- dgr_fmt(seq(-30,25, by=5))
    
    # create a small dataframe to represent legend symbol for 2014 Temperature
    legend_data <- data.frame(x=seq(175,182),y=rnorm(8,15,2))
    
    p <- ggplot(Past, aes(newDay, Temp)) +
      theme(plot.background = element_blank(),
            panel.grid.minor = element_blank(),
            panel.grid.major = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank(),
            axis.ticks = element_blank(),
            #axis.text = element_blank(),  
            axis.title = element_blank()) +
      geom_linerange(Past, mapping=aes(x=newDay, ymin=lower, ymax=upper), colour = "wheat2", alpha=.1)
    
    #print(p)
    
    p <- p + 
      geom_linerange(Past, mapping=aes(x=newDay, ymin=avg_lower, ymax=avg_upper), colour = "wheat4")
    
    #print(p)
    
    p <- p + 
      geom_line(Present, mapping=aes(x=newDay, y=Temp, group=1)) +
      geom_vline(xintercept = 0, colour = "wheat4", linetype=1, size=1)
    
    #print(p)
    
    p <- p + 
      geom_hline(yintercept = -30, colour = "white", linetype=1) +
      geom_hline(yintercept = -25, colour = "white", linetype=1) +
      geom_hline(yintercept = -20, colour = "white", linetype=1) +
      geom_hline(yintercept = -15, colour = "white", linetype=1) +
      geom_hline(yintercept = -10, colour = "white", linetype=1) +
      geom_hline(yintercept = -5, colour = "white", linetype=1) +
      geom_hline(yintercept = 0, colour = "white", linetype=1) +
      geom_hline(yintercept = 5, colour = "white", linetype=1) +
      geom_hline(yintercept = 10, colour = "white", linetype=1) +
      geom_hline(yintercept = 15, colour = "white", linetype=1) +
      geom_hline(yintercept = 20, colour = "white", linetype=1) +
      geom_hline(yintercept = 25, colour = "white", linetype=1)
    
    
    #print(p)
    
    p <- p + 
      geom_vline(xintercept = 31, colour = "wheat4", linetype=3, size=.5) +
      geom_vline(xintercept = 59, colour = "wheat4", linetype=3, size=.5) +
      geom_vline(xintercept = 90, colour = "wheat4", linetype=3, size=.5) +
      geom_vline(xintercept = 120, colour = "wheat4", linetype=3, size=.5) +
      geom_vline(xintercept = 151, colour = "wheat4", linetype=3, size=.5) +
      geom_vline(xintercept = 181, colour = "wheat4", linetype=3, size=.5) +
      geom_vline(xintercept = 212, colour = "wheat4", linetype=3, size=.5) +
      geom_vline(xintercept = 243, colour = "wheat4", linetype=3, size=.5) +
      geom_vline(xintercept = 273, colour = "wheat4", linetype=3, size=.5) +
      geom_vline(xintercept = 304, colour = "wheat4", linetype=3, size=.5) +
      geom_vline(xintercept = 334, colour = "wheat4", linetype=3, size=.5) +
      geom_vline(xintercept = 365, colour = "wheat4", linetype=3, size=.5) 
    
    #print(p)
    
    p <- p +
      coord_cartesian(ylim = c(-30,25)) +
      scale_y_continuous(breaks = seq(-30,25, by=5), labels = a) +
      scale_x_continuous(expand = c(0, 0), 
                         breaks = c(15,45,75,105,135,165,195,228,258,288,320,350),
                         labels = c("January", "February", "March", "April",
                                    "May", "June", "July", "August", "September",
                                    "October", "November", "December"))
    
  })
  
  #-------------------------------------------------------------------------------------Rainfall
  output$annualRainfall <- renderPlot({
    
    mTt<-subset(stnDatMaster, stnDatMaster$stN %in% input$catch)
    
    mTt1<-mTt[,1:4]
    mTt1$Year<-as.numeric(substr(mTt[,1],1,4))
    mTt1$Month<-as.numeric(substr(mTt[,1],6,7))
    mTt1$Day<-as.numeric(substr(mTt[,1],9,10))
    dat2<-mTt1[,-1]
    
    # rename variables
    names(dat2)[3] <- "Runoff"
    
    
    datR<-cbind.data.frame(data.frame(dat2[,6]),data.frame(dat2[,5]),data.frame(dat2[,4]),data.frame(dat2[,1])); names(datR)<-c( "Day", "Month", "Year","Rainfall")
    
    Past <- datR%>%
      group_by(Year, Month)%>%
      arrange(Month)%>%
      ungroup()%>%
      group_by(Year)%>%
      mutate(newDay = seq(1, length(Day)))%>%   # label days as 1:365 (will represent x-axis)         
      ungroup()%>%
      filter(Rainfall != -99 & Year != 2011)%>%     # filter out missing data (identified with '-99' value) & current year data
      group_by(newDay)%>%
      mutate(upper = max(Rainfall), # identify max value for each day
             lower = min(Rainfall), # identify min value for each day
             avg = mean(Rainfall),  # calculate mean value for each day
             se = sd(Rainfall)/sqrt(length(Rainfall))) %>%  # calculate standard error of mean
      mutate(avg_upper = avg+(2.101*se),  # calculate 95% CI for mean
             avg_lower = avg-(2.101*se)) %>%  # calculate 95% CI for mean
      ungroup()
    
    # create dataframe that represents current year data
    Present <- datR%>%
      group_by(Year, Month)%>%
      arrange(Month)%>%
      ungroup()%>%
      group_by(Year)%>%
      mutate(newDay = seq(1, length(Day)))%>%  # create matching x-axis as historical data
      ungroup()%>%
      filter(Rainfall != -99 & Year == 2011)  # filter out missing data & select current year data
    
    
    # create dataframe that represents the lowest temp for each day for the historical data
    PastLows <- Past %>%
      group_by(newDay) %>%
      summarise(Pastlow = min(Rainfall)) # identify lowest temp for each day from 1995-2013
    
    # create dataframe that identifies the days in 2014 in which the temps were lower than all previous 19 years
    PresentLows <- Present %>%
      left_join(PastLows) %>%  # merge historical lows to current year low data
      mutate(record = ifelse(Rainfall<Pastlow, "Y", "N")) %>% # identifies if current year was record low
      filter(record == "Y")  # filter for days that represent current year record lows
    
    # create dataframe that represents the highest temp for each day for the historical data
    PastHighs <- Past %>%
      group_by(newDay) %>%
      summarise(Pasthigh = max(Rainfall))  # identify highest temp for each day from 1995-2013
    
    # create dataframe that identifies the days in 2014 in which the temps were higher than all previous 19 years
    PresentHighs <- Present %>%
      left_join(PastHighs) %>%  # merge historical highs to current year low data
      mutate(record = ifelse(Rainfall>Pasthigh, "Y", "N")) %>% # identifies if current year was record high
      filter(record == "Y")  # filter for days that represent current year record highs
    #Lastly, to create the appropriate y-axis, I need to generate a function to turn the y-axis labels into the appropriate format with the degree symbol (0) after the numbers. I then create a variable that represents the y-axis labels I want to display using seq() and apply the degree formatting function to these labels. I save it as variable "a" to be used later in my ggplot code.
    
    #The final data I need to process is to create a small line that will represent the legend symbol for the 2014 Temperature. In Tufte's illustration, the highs and lows data for each day were available so it made sense to represent the current year bars; however, in my case I only have access to the average daily temperature for each day. Since I don't have a range of data for the individual days in 2014, I can only represent the data by a single line. This, ultimately, is why my legend differs slightly compared to the original graphic.
    
    # function to turn y-axis labels into degree formatted values
    dgr_fmt <- function(x, ...) {
      parse(text = paste(x, "*degree", sep = ""))
    }
    
    # create y-axis variable
    a <- dgr_fmt(seq(-10,110, by=10))
    
    # create a small dataframe to represent legend symbol for 2014 Temperature
    legend_data <- data.frame(x=seq(175,182),y=rnorm(8,15,2))
    
    p <- ggplot(Past, aes(newDay, Rainfall)) +
      theme(plot.background = element_blank(),
            panel.grid.minor = element_blank(),
            panel.grid.major = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank(),
            axis.ticks = element_blank(),
            #axis.text = element_blank(),  
            axis.title = element_blank()) +
      geom_linerange(Past, mapping=aes(x=newDay, ymin=lower, ymax=upper), colour = "wheat2", alpha=.1)
    
    #print(p)
    
    p <- p + 
      geom_linerange(Past, mapping=aes(x=newDay, ymin=avg_lower, ymax=avg_upper), colour = "wheat4")
    
    #print(p)
    
    p <- p + 
      geom_line(Present, mapping=aes(x=newDay, y=Rainfall, group=1)) +
      geom_vline(xintercept = 0, colour = "wheat4", linetype=1, size=1)
    
    #print(p)
    
    p <- p + 
      geom_hline(yintercept = -10, colour = "white", linetype=1) +
      geom_hline(yintercept = 0, colour = "white", linetype=1) +
      geom_hline(yintercept = 10, colour = "white", linetype=1) +
      geom_hline(yintercept = 20, colour = "white", linetype=1) +
      geom_hline(yintercept = 30, colour = "white", linetype=1) +
      geom_hline(yintercept = 40, colour = "white", linetype=1) +
      geom_hline(yintercept = 50, colour = "white", linetype=1) +
      geom_hline(yintercept = 60, colour = "white", linetype=1) +
      geom_hline(yintercept = 70, colour = "white", linetype=1) +
      geom_hline(yintercept = 80, colour = "white", linetype=1) +
      geom_hline(yintercept = 90, colour = "white", linetype=1) +
      geom_hline(yintercept = 100, colour = "white", linetype=1) +
      geom_hline(yintercept = 110, colour = "white", linetype=1)
    
    
    #print(p)
    
    p <- p + 
      geom_vline(xintercept = 31, colour = "wheat4", linetype=3, size=.5) +
      geom_vline(xintercept = 59, colour = "wheat4", linetype=3, size=.5) +
      geom_vline(xintercept = 90, colour = "wheat4", linetype=3, size=.5) +
      geom_vline(xintercept = 120, colour = "wheat4", linetype=3, size=.5) +
      geom_vline(xintercept = 151, colour = "wheat4", linetype=3, size=.5) +
      geom_vline(xintercept = 181, colour = "wheat4", linetype=3, size=.5) +
      geom_vline(xintercept = 212, colour = "wheat4", linetype=3, size=.5) +
      geom_vline(xintercept = 243, colour = "wheat4", linetype=3, size=.5) +
      geom_vline(xintercept = 273, colour = "wheat4", linetype=3, size=.5) +
      geom_vline(xintercept = 304, colour = "wheat4", linetype=3, size=.5) +
      geom_vline(xintercept = 334, colour = "wheat4", linetype=3, size=.5) +
      geom_vline(xintercept = 365, colour = "wheat4", linetype=3, size=.5) 
    
    #print(p)
    
    p <- p +
      coord_cartesian(ylim = c(-10,110)) +
      scale_y_continuous(breaks = seq(-10,110, by=10), labels = a) +
      scale_x_continuous(expand = c(0, 0), 
                         breaks = c(15,45,75,105,135,165,195,228,258,288,320,350),
                         labels = c("January", "February", "March", "April",
                                    "May", "June", "July", "August", "September",
                                    "October", "November", "December"))
    
    
  })
  
  #-------------------------------------Runoff
  
  output$annualFlow <- renderPlot({
    
    mTt<-subset(stnDatMaster, stnDatMaster$stN %in% input$catch)
    
    mTt<-subset(stnDatMaster, stnDatMaster$stN %in% input$catch)
    
    mTt1<-mTt[,1:4]
    mTt1$Year<-as.numeric(substr(mTt[,1],1,4))
    mTt1$Month<-as.numeric(substr(mTt[,1],6,7))
    mTt1$Day<-as.numeric(substr(mTt[,1],9,10))
    dat2<-mTt1[,-1]
    
    # rename variables
    names(dat2)[3] <- "Runoff"
    
    
    datD<-cbind.data.frame(data.frame(dat2[,6]),data.frame(dat2[,5]),data.frame(dat2[,4]),data.frame(dat2[,3])); names(datD)<-c( "Day", "Month", "Year","Runoff")
    
    
    Past <- datD%>%
      group_by(Year, Month)%>%
      arrange(Month)%>%
      ungroup()%>%
      group_by(Year)%>%
      mutate(newDay = seq(1, length(Day)))%>%   # label days as 1:365 (will represent x-axis)         
      ungroup()%>%
      filter(Runoff != -99 & Year != 2011)%>%     # filter out missing data (identified with '-99' value) & current year data
      group_by(newDay)%>%
      mutate(upper = max(Runoff), # identify max value for each day
             lower = min(Runoff), # identify min value for each day
             avg = mean(Runoff),  # calculate mean value for each day
             se = sd(Runoff)/sqrt(length(Runoff))) %>%  # calculate standard error of mean
      mutate(avg_upper = avg+(2.101*se),  # calculate 95% CI for mean
             avg_lower = avg-(2.101*se)) %>%  # calculate 95% CI for mean
      ungroup()
    
    # create dataframe that represents current year data
    Present <- datD%>%
      group_by(Year, Month)%>%
      arrange(Month)%>%
      ungroup()%>%
      group_by(Year)%>%
      mutate(newDay = seq(1, length(Day)))%>%  # create matching x-axis as historical data
      ungroup()%>%
      filter(Runoff != -99 & Year == 2011)  # filter out missing data & select current year data
    
    
    # create dataframe that represents the lowest temp for each day for the historical data
    PastLows <- Past %>%
      group_by(newDay) %>%
      summarise(Pastlow = min(Runoff)) # identify lowest temp for each day from 1995-2013
    
    # create dataframe that identifies the days in 2014 in which the temps were lower than all previous 19 years
    PresentLows <- Present %>%
      left_join(PastLows) %>%  # merge historical lows to current year low data
      mutate(record = ifelse(Runoff<Pastlow, "Y", "N")) %>% # identifies if current year was record low
      filter(record == "Y")  # filter for days that represent current year record lows
    
    # create dataframe that represents the highest temp for each day for the historical data
    PastHighs <- Past %>%
      group_by(newDay) %>%
      summarise(Pasthigh = max(Runoff))  # identify highest temp for each day from 1995-2013
    
    # create dataframe that identifies the days in 2014 in which the temps were higher than all previous 19 years
    PresentHighs <- Present %>%
      left_join(PastHighs) %>%  # merge historical highs to current year low data
      mutate(record = ifelse(Runoff>Pasthigh, "Y", "N")) %>% # identifies if current year was record high
      filter(record == "Y")  # filter for days that represent current year record highs
    #Lastly, to create the appropriate y-axis, I need to generate a function to turn the y-axis labels into the appropriate format with the degree symbol (o) after the numbers. I then create a variable that represents the y-axis labels I want to display using seq() and apply the degree formatting function to these labels. I save it as variable "a" to be used later in my ggplot code.
    
    #The final data I need to process is to create a small line that will represent the legend symbol for the 2014 Temperature. In Tufte's illustration, the highs and lows data for each day were available so it made sense to represent the current year bars; however, in my case I only have access to the average daily temperature for each day. Since I don't have a range of data for the individual days in 2014, I can only represent the data by a single line. This, ultimately, is why my legend differs slightly compared to the original graphic.
    
    # function to turn y-axis labels into degree formatted values
    dgr_fmt <- function(x, ...) {
      parse(text = paste(x, "*degree", sep = ""))
    }
    
    # create y-axis variable
    a <- dgr_fmt(seq(-5,40, by=5))
    
    # create a small dataframe to represent legend symbol for 2014 Temperature
    legend_data <- data.frame(x=seq(175,182),y=rnorm(8,15,2))
    
    p <- ggplot(Past, aes(newDay, Runoff)) +
      theme(plot.background = element_blank(),
            panel.grid.minor = element_blank(),
            panel.grid.major = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank(),
            axis.ticks = element_blank(),
            #axis.text = element_blank(),  
            axis.title = element_blank()) +
      geom_linerange(Past, mapping=aes(x=newDay, ymin=lower, ymax=upper), colour = "wheat2", alpha=.1)
    
    #print(p)
    
    p <- p + 
      geom_linerange(Past, mapping=aes(x=newDay, ymin=avg_lower, ymax=avg_upper), colour = "wheat4")
    
    #print(p)
    
    p <- p + 
      geom_line(Present, mapping=aes(x=newDay, y=Runoff, group=1)) +
      geom_vline(xintercept = 0, colour = "wheat4", linetype=1, size=1)
    
    #print(p)
    
    p <- p + 
      geom_hline(yintercept = -5, colour = "white", linetype=1) +
      geom_hline(yintercept = 0, colour = "white", linetype=1) +
      geom_hline(yintercept = 5, colour = "white", linetype=1) +
      geom_hline(yintercept = 10, colour = "white", linetype=1) +
      geom_hline(yintercept = 15, colour = "white", linetype=1) +
      geom_hline(yintercept = 20, colour = "white", linetype=1) +
      geom_hline(yintercept = 25, colour = "white", linetype=1) +
      geom_hline(yintercept = 30, colour = "white", linetype=1) +
      geom_hline(yintercept = 35, colour = "white", linetype=1) +
      geom_hline(yintercept = 40, colour = "white", linetype=1)
    
    
    
    #print(p)
    
    p <- p + 
      geom_vline(xintercept = 31, colour = "wheat4", linetype=3, size=.5) +
      geom_vline(xintercept = 59, colour = "wheat4", linetype=3, size=.5) +
      geom_vline(xintercept = 90, colour = "wheat4", linetype=3, size=.5) +
      geom_vline(xintercept = 120, colour = "wheat4", linetype=3, size=.5) +
      geom_vline(xintercept = 151, colour = "wheat4", linetype=3, size=.5) +
      geom_vline(xintercept = 181, colour = "wheat4", linetype=3, size=.5) +
      geom_vline(xintercept = 212, colour = "wheat4", linetype=3, size=.5) +
      geom_vline(xintercept = 243, colour = "wheat4", linetype=3, size=.5) +
      geom_vline(xintercept = 273, colour = "wheat4", linetype=3, size=.5) +
      geom_vline(xintercept = 304, colour = "wheat4", linetype=3, size=.5) +
      geom_vline(xintercept = 334, colour = "wheat4", linetype=3, size=.5) +
      geom_vline(xintercept = 365, colour = "wheat4", linetype=3, size=.5) 
    
    #print(p)
    
    p <- p +
      coord_cartesian(ylim = c(-5,40)) +
      scale_y_continuous(breaks = seq(-5,40, by=5), labels = a) +
      scale_x_continuous(expand = c(0, 0), 
                         breaks = c(15,45,75,105,135,165,195,228,258,288,320,350),
                         labels = c("January", "February", "March", "April",
                                    "May", "June", "July", "August", "September",
                                    "October", "November", "December"))
    
  })
  
})
